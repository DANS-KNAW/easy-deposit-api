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

import java.nio.file.StandardWatchEventKinds.{ ENTRY_CREATE, ENTRY_DELETE }
import java.nio.file.{ WatchEvent, WatchKey, WatchService }

import better.files.File
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.error._

import scala.util.Try

object WorkerThread extends DebugEnhancedLogging {

  val stagedBaseDir = File("target")

  def main(args: Array[String]): Unit = {

    // TODO create watchService as first action in EasyDepositApiService.start
    //  close it as last action in EasyDepositApiService.stop
    stagedBaseDir.watchService.apply { watchService =>
      stagedBaseDir.register(watchService, Seq(ENTRY_CREATE, ENTRY_DELETE))
      takeLoop(watchService)// TODO in a sub-thread created by EasyDepositApiService.start
    }
  }

  def takeLoop(watchService: WatchService): Unit = {
    handleAbortedByHardShutdown()
    while ( {
      // https://howtodoinjava.com/java8/java-8-watchservice-api-tutorial/
      // take, because polling skips staged-upload-dirs created and deleted within a single cycle
      Try(watchService.take())
        .map(handleWatchKey)
        .doIfFailure { case e =>
          // does not get here on SIGINT despite InterruptedException thrown by take
          // TODO would it get a ClosedWatchServiceException when the parent thread is the culprit?
          logger.error(s"terminating ${ e.getMessage }")
        }
        .getOrElse(false)
    }) {}
  }

  private def handleWatchKey(watchKey: WatchKey): Boolean = {
    watchKey.pollEvents.stream.forEach(handleEvent(_)) // does just one event
    watchKey.reset() // next pending event
  }

  private def handleEvent(event: WatchEvent[_]): Unit = {
    val stagedDepositDir = stagedBaseDir / event.context.toString
    (event.kind(), getDepositState(stagedDepositDir.name)) match {
      case (ENTRY_CREATE, State.finalizing) => finalizeSubmit(stagedDepositDir)
      case (ENTRY_DELETE, State.draft) => calculateShas(stagedDepositDir)
      case _ => // either completed finalizeSubmit or started upload
    }
  }

  private def handleAbortedByHardShutdown(): Unit = {
    stagedBaseDir.list.foreach { abortedDir =>
      getDepositState(abortedDir.name) match {
        case State.draft => abortedUpload(abortedDir)
        case State.finalizing => abortedSubmit(abortedDir)
        case state => logger.error(s"Staged garbage : $state $abortedDir")
      }
    }
    logger.info(s"So far what was found at startup in $stagedBaseDir")
  }

  private def abortedSubmit(stagedSubmitDir: File): Unit = {
    logger.info(s"Found submit aborted by hard shutdown: $stagedSubmitDir")
    deleteContent(stagedSubmitDir) // clean up for a fresh submit
    finalizeSubmit(stagedSubmitDir)
  }

  def deleteContent(dir: File): Unit = {
    dir.list.foreach { f =>
      println(s"deleting $f")
      if (f.isDirectory) deleteContent(f)
      f.delete()
    }
  }

  private def abortedUpload(stagedUploadDir: File): Unit = {
    logger.info(s"Found upload aborted by hard shutdown: $stagedUploadDir")
    // * the last multipart file may be incomplete,
    //   delete all to keep it simple (the target is no longer known anyway)
    // * some multipart files may have been moved into the draft bag
    //   a running watchService can pick up the delete of the staged directory for SHA calculations
    // * not completed SHA calculations for completed uploads won't be detected at restart
    stagedUploadDir
      .list
      .filter(_.name.toLowerCase.startsWith("multipart"))
      .foreach(_.delete())
    stagedUploadDir.delete()
  }

  /**
   * @param stagedDir created with EasyDepositApiApp.getStagedDir
   *                  format: [user-id]-[UUID]-[temp-name]
   */
  private def getDepositState(stagedDir: String) = {
    // TODO implement stub: get state from <drafts>/<user>/<UUID>/deposit.properties
    stagedDir.toString match {
      case "delete-me" => State.draft
      case "submit-me" => State.finalizing
      case _ => State.rejected
    }
  }

  private def calculateShas(stagedUploadDir: File): Unit = {
    logger.info(s"Calculate missing SHA's for: $stagedUploadDir")
    // TODO implement SHA stub
  }

  private def finalizeSubmit(stagedSubmitDir: File): Unit = {
    logger.info(s"Prepare and move to ingest-flow-inbox: $stagedSubmitDir")
    // TODO implement submit stub
  }
}
