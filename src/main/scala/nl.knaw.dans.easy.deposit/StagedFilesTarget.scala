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
import java.util.UUID

import better.files.File
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }

/**
 * @param draftBag    the bag that receives the staged files as payload
 * @param destination relative location in the bag's data directory
 */
case class StagedFilesTarget(id: UUID, draftBag: DansBag, destination: Path) extends DebugEnhancedLogging {

  private val dataFiles = DataFiles(draftBag)

  /**
   * Moves files from stagingDir to draftBag, after deleting each file in the bag as soon as it would be overwritten.
   *
   * @param stagingDir the temporary container for files, unique per request, same mount as draftBag
   * @return
   */
  def moveAllFrom(stagingDir: File): Try[Unit] = {
    // though currently we don't create fetch files, let us not run into trouble whenever we do
    lazy val fetchFiles = draftBag.fetchFiles
      .map(fetchFile =>
        draftBag.data.relativize(fetchFile.file)
      )

    def cleanUp(bagRelativePath: Path) = {
      val oldFile = draftBag.data / bagRelativePath.toString
      if (oldFile.exists) {
        logger.info(s"[$id] removing payload file $bagRelativePath to be replaced by the newly uploaded file")
        draftBag.removePayloadFile(bagRelativePath)
      }
      else if (fetchFiles.contains(bagRelativePath)) {
        logger.info(s"[$id] removing fetch file $bagRelativePath to be replaced by the newly uploaded file")
        draftBag.removeFetchItem(bagRelativePath)
      }
      else Success(())
    }

    stagingDir.walk()
      .withFilter(!_.isDirectory)
      .map { stagedFile =>
        val bagRelativePath = destination.resolve(stagingDir.relativize(stagedFile))
        for {
          _ <- cleanUp(bagRelativePath)
          _ = logger.info(s"[$id] moving uploaded files in $stagingDir to bag payload ${ draftBag.data }")
          _ <- dataFiles.move(stagedFile, bagRelativePath)
        } yield ()
      }.failFastOr(draftBag.save)
  }
}
