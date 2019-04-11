/**
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.deposit

import java.nio.file.{ FileAlreadyExistsException, Path }

import better.files.File
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.bag.ImportOption.ATOMIC_MOVE
import nl.knaw.dans.easy.deposit.Errors.OverwriteException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

/**
 * @param draftBag    the bag that receives the staged files as payload
 * @param destination relative location in the bag's data directory
 */
case class StagedFilesTarget(draftBag: DansBag, destination: Path) extends DebugEnhancedLogging {

  /**
   * Moves files from stagingDir to draftBag if no files in the bag would be overwritten.
   *
   * @param stagingDir the temporary container for files, unique per request, same mount as draftBag
   * @return
   */
  def moveAllFrom(stagingDir: File): Try[Unit] = {
    // read files.xml at most once, not at all if the first file appears to exist as payload
    lazy val fetchFiles = draftBag.fetchFiles.map(_.file)

    val absDestination = draftBag.baseDir / destination.toString

    def sourceToTarget(sourceFile: File) = {
      val bagRelativePath = destination.resolve(stagingDir.relativize(sourceFile))
      val isPayload = (draftBag.data / bagRelativePath.toString).exists
      lazy val isFetchItem = fetchFiles.contains(draftBag.data / bagRelativePath.toString)
      if (logger.underlying.isDebugEnabled)
        logger.debug(s"Checking existence of $bagRelativePath isPayload=$isPayload isFetchItem=$isFetchItem")
      if (isPayload || isFetchItem)
        Failure(new FileAlreadyExistsException(bagRelativePath.toString))
      else Success(sourceFile -> bagRelativePath)
    }

    val (successesOfSourceTarget, duplicates) = stagingDir
      .list
      .map(sourceToTarget)
      .partition(triedTuple => triedTuple.isSuccess)

    if (duplicates.nonEmpty)
      collectExistingFiles(duplicates)
    else {
      logger.info(s"Moving from staging [$stagingDir] to [$absDestination]")
      successesOfSourceTarget.toStream.map {
        _.flatMap { case (src: File, target: Path) => draftBag
          .addPayloadFile(src, target)(ATOMIC_MOVE)
          .recoverWith { case _: FileAlreadyExistsException =>
            logger.warn(s"A concurrent PUT created $target, upload continues with the rest of the files.")
            Success(draftBag)
          }
        }
      }.failFastOr(draftBag.save)
    }
  }

  private def collectExistingFiles(duplicates: Iterator[Try[(File, Path)]]): Failure[Nothing] = {
    val msg = duplicates
      .map(_.failed.getOrElse(new Exception("should not get here")).getMessage)
      .mkString(", ")
    Failure(OverwriteException("The following file(s) already exist on the server: " + msg))
  }
}
