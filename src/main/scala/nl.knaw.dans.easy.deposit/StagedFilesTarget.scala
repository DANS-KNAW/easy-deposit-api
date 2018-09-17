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

import java.nio.file.Path

import better.files.File
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }

/**
 * @param draftBag    the bag that receives the staged files as payload
 * @param destination relative location in the bag's data directory
 */
case class StagedFilesTarget(draftBag: DansBag, destination: Path) extends DebugEnhancedLogging {

  /**
   * @param stagingDir the temporary container for files, unique per request, same mount as draftBag
   * @return
   */
  def takeAllFrom(stagingDir: File): Try[Any] = {
    logger.info(s"moving from staging [$stagingDir] to ${ draftBag.baseDir / destination.toString }")

    def moveToDraft(file: File): Try[Unit] = {
      val bagRelativePath = destination.resolve(stagingDir.relativize(file))
      logger.debug(s"moving to $bagRelativePath")
      // check fetch files just in case pruning gets implemented, for example after rejecting or un-publishing
      val exists =
        (draftBag.data / bagRelativePath.toString).exists ||
        draftBag.fetchFiles.map(_.file).contains(destination)// TODO check and provide proof
      for {
        _ <- if (exists) draftBag.removePayloadFile(bagRelativePath)
             else Success(())
        _ <- draftBag.addPayloadFile(file, bagRelativePath)
      } yield ()
    }

    stagingDir
      .walk()
      .filter(!_.isDirectory)
      .map(moveToDraft)
      .find(_.isFailure)
      .getOrElse(draftBag.save)
  }
}
