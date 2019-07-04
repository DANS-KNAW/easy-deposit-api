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
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

object WorkerThread extends DebugEnhancedLogging {

  val stagedBaseDir = File("target") // TODO make flexible for unit tests / use EasyDepositApiApp.stagedBaseDir

  def main(args: Array[String]): Unit = {

    // TODO create watchService as first action in EasyDepositApiService.start
    //  close it as last action in EasyDepositApiService.stop
    stagedBaseDir.watchService.apply { watchService =>
      stagedBaseDir.register(watchService, Seq(ENTRY_CREATE, ENTRY_DELETE))
      handleWatchService(watchService) // TODO in a sub-thread created by ServiceStarter.start
    }
  }

  def handleWatchService(watchService: WatchService): Unit = {
    // https://howtodoinjava.com/java8/java-8-watchservice-api-tutorial/
    // take, because polling skips staged-upload-dirs created and deleted within a single cycle
    handleAbortedByHardShutdown()
    Iterator.continually(()).foreach { _ =>
      Try(watchService.take())
        .map(handleWatchKey)
        .doIfFailure { case e =>
          // does not get here on SIGINT despite InterruptedException thrown by take
          // TODO would it get a ClosedWatchServiceException when the parent thread is the culprit?
          logger.error(s"terminating WorkerThread ${ e.getMessage }")
        }
        .getOrElse(false)
    }
    logger.info("WorkerThread terminated")
  }

  private def handleWatchKey(watchKey: WatchKey): Boolean = {
    watchKey.pollEvents.stream.forEach(handleWatchEvent(_)) // does just one event
    watchKey.reset() // next pending event
  }

  private def handleWatchEvent(event: WatchEvent[_]): Unit = {
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

    // delete and create the directory again could trigger the watchService to perform the submit
    // but a new hard shutdown could happen between the two actions
    // then the submit would no longer be noticed
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
    // * some uploaded files may have been moved into the draft bag
    // * not completed SHA calculations for completed uploads won't be detected at restart
    logger.info(s"Found upload aborted by hard shutdown: $stagedUploadDir")

    // we don't know the location in the bag any more for the remaining uploaded files
    deleteContent(stagedUploadDir)

    // create an event that triggers a running watchService to perform SHA calculations
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
    // TODO implement SHA stub: https://drivenbydata.atlassian.net/browse/EASY-2157
    //
    // add methods to dans-bag-lib
    // - add payload files without checksums
    // - calculate missing checksums
    // call the first by the servlet, the latter in this stub
    // the deposit-ui should present missing checksums as ???
  }

  private def finalizeSubmit(stagedSubmitDir: File): Unit = {
    logger.info(s"Prepare and move to ingest-flow-inbox: $stagedSubmitDir")
    // TODO implement submit stub: https://drivenbydata.atlassian.net/browse/EASY-2158
    //
    // replace https://github.com/DANS-KNAW/easy-deposit-api/blob/ea0abe91ce9474ca3de6a171501257b5b1827439/src/main/scala/nl.knaw.dans.easy.deposit/EasyDepositApiApp.scala#L181
    // with _ <- stateManager.changeState(StateInfo(State.finalizing, "Preparing deposit for submission."))
    // note that the disposableStagedDir should not get deleted at successful completion of the request
    //
    // call Submitter.submit in this stub and make a few changes:
    // drop https://github.com/DANS-KNAW/easy-deposit-api/blob/b8a80248f95a8441553822b9888ac98a5251618a/src/main/scala/nl.knaw.dans.easy.deposit/Submitter.scala#L93
    // set state in staged deposit to SUBMITTED before moving
    // set state in draft deposit to SUBMITTED after moving
  }
}
