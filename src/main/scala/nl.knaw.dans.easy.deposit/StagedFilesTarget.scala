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
import nl.knaw.dans.bag.ImportOption.ATOMIC_MOVE
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }

/**
 * @param draftBag    the bag that receives the staged files as payload
 * @param destination relative location in the bag's data directory
 */
case class StagedFilesTarget(draftBag: DansBag, destination: Path) extends DebugEnhancedLogging {

  /**
   * Moves files from stagingDir to draftBag, after deleting each file in the bag as soon as it would be overwritten.
   *
   * @param stagingDir the temporary container for files, unique per request, same mount as draftBag
   * @return
   */
  def moveAllFrom(stagingDir: File): Try[Unit] = {
    // though currently we don't create fetch files, let us not run into trouble whenever we do
    val fetchFiles = draftBag.fetchFiles
      .map(fetchFile =>
        draftBag.data.relativize(fetchFile.file)
      )
    stagingDir.walk()
      .filter(!_.isDirectory)
      .map { stagedFile =>
        val bagRelativePath = destination.resolve(stagingDir.relativize(stagedFile))
        val triedCleanup = if ((draftBag.data / bagRelativePath.toString).exists)
                             draftBag.removePayloadFile(bagRelativePath)
                           else if (fetchFiles.contains(bagRelativePath))
                                  draftBag.removeFetchItem(bagRelativePath)
                           else Success(())
        // a hard shutdown at this point causes a file being deleted without having been overwritten
        triedCleanup.flatMap(_ =>
          draftBag.addPayloadFile(stagedFile, bagRelativePath)(ATOMIC_MOVE)
        )
      }.failFastOr(draftBag.save)
  }
}
