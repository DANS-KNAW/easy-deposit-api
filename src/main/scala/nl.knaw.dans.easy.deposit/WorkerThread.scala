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
import java.nio.file.{ WatchEvent, WatchKey }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.Errors._
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

case class WorkerThread(apiApp: EasyDepositApiApp) extends Runnable with AutoCloseable with DebugEnhancedLogging {

  private val watchedDir = apiApp.stagedBaseDir
  private val watchService = watchedDir.newWatchService // TODO https://github.com/pathikrit/better-files#file-monitoring
  watchedDir.register(watchService, Seq(ENTRY_CREATE, ENTRY_DELETE))

  // don't delay startup (so process in run) nor interfere with new events (so get all before run)
  private val aborted = watchedDir.list.toArray

  // otherwise early events (by users or handleAbortedByHardShutdown) are not picked up
  Thread.sleep(50) // TODO How to check the file system is ready to detect/notify watched events?

  // TODO start the thread as first action in EasyDepositApiService.start
  //  Close watchService as last action in EasyDepositApiService.stop, but how/when?
  //  How to detect no events are pending and the service can be stopped?
  //  We can count directories for submissions. The delete events that trigger SHA calculation
  //  might still be pending but directories are gone.
  //  How to terminate the caller in case of a not expected fatal error?
  override def run(): Unit = {
    aborted.foreach { handleAbortedByHardShutdown }
    // https://howtodoinjava.com/java8/java-8-watchservice-api-tutorial/
    // used take instead, because polling skips staged-upload-dirs created and deleted within a single cycle
    Iterator.continually(()).foreach { _ =>
      Try(watchService.take())
        .map(handleWatchKey)
        .doIfFailure { case e =>
          // does not get here on SIGINT despite take claiming to throw InterruptedException
          // TODO would it get a ClosedWatchServiceException when the parent thread is the culprit?
          logger.error(s"terminating WatchService ${ e.getMessage }".toString)
        }
        .getOrElse(false)
    }
    logger.info("WatchService terminated")
  }

  private def handleWatchKey(watchKey: WatchKey): Boolean = {
    watchKey.pollEvents.stream.forEach(handleWatchEvent(_)) // does just one event
    watchKey.reset() // next pending event
  }

  private def handleWatchEvent(event: WatchEvent[_]): Unit = {
    val stagedDepositDir = watchedDir / event.context.toString
    (event.kind(), getDepositState(stagedDepositDir)) match {
      case (ENTRY_CREATE, Success(State.draft)) =>
        logger.info(s"Detected upload started event: $stagedDepositDir".toString)
      case (ENTRY_DELETE, Success(State.finalizing)) =>
        logger.info(s"Detected submit completed event: $stagedDepositDir".toString)
      case (ENTRY_DELETE, Success(State.draft)) =>
        logger.info(s"Detected upload completed event: $stagedDepositDir".toString)
        calculateShas(stagedDepositDir)
      case (ENTRY_CREATE, Success(State.finalizing)) =>
        logger.info(s"Detected submit started event: $stagedDepositDir".toString)
        finalizeSubmit(stagedDepositDir)
      case (_, Success(state)) => logNotExpectedState(stagedDepositDir, state)
      case (_, Failure(e)) => logger.warn(e.getMessage)
      case _ => // either completed finalizeSubmit or started upload
    }
  }

  private def handleAbortedByHardShutdown(abortedDir: File) = {
    getDepositState(abortedDir) match {
      case Success(State.draft) =>
        logger.info(s"Found upload aborted by hard shutdown: ${ abortedDir.toString() }".toString)
        // Ignore uploaded files not yet moved into a bag. They may be incomplete and we don't know the target anymore.
        abortedDir.delete() // triggers a running watchService
      case Success(State.finalizing) =>
        logger.info(s"Found submit aborted by hard shutdown: $abortedDir".toString)
        // Don't delete and create the directory again to trigger the watch service.
        // Another hard shutdown could occur in between.
        abortedDir.list.foreach { _.delete() }
        finalizeSubmit(abortedDir)
      case Success(state) => logNotExpectedState(abortedDir, state)
      case f @ (Failure(InvalidStagedDirException()) |
                Failure(NoSuchDepositException(_, _, _)) |
                Failure(InvalidPropertyException(_, _, _)) |
                Failure(PropertyNotFoundException(_, _))) =>
        logger.warn(s"$abortedDir : ${ f.toString }".toString)
      case Failure(e: Throwable) => logger.warn(s"$abortedDir".toString, e)
    }
  }

  private def logNotExpectedState(abortedDir: File, state: State): Unit = {
    logger.warn(s"Not expected deposit state: $state $abortedDir".toString)
  }

  /**
   * @param stagedDir created with EasyDepositApiApp.getStagedDir
   *                  format: [userId]-[UUID]-[tempName]
   */
  private def getDepositState(stagedDir: File): Try[State] = {
    val userPlusUuid = stagedDir.name.replaceAll("-[^-]+$", "") // trim [temp-name]
    val namePlusUuidLength = userPlusUuid.length
    val uuidStringLength = 36
    if (namePlusUuidLength < uuidStringLength + 2)
      Failure(InvalidStagedDirException())
    else Try { UUID.fromString(userPlusUuid.substring(namePlusUuidLength - uuidStringLength)) }
      .recoverWith { case _ => Failure(InvalidStagedDirException()) }
      .flatMap { uuid =>
        val userName = userPlusUuid.substring(0, namePlusUuidLength - uuidStringLength - 1)
        apiApp
          .getDepositState(userName, uuid)
          .map(_.state)
      }
  }

  private def calculateShas(stagedUploadDir: File): Unit = {
    logger.info(s"STUB: Calculate missing SHA's for: $stagedUploadDir".toString)
    // TODO implement SHA stub: https://drivenbydata.atlassian.net/browse/EASY-2157
    //
    // add methods to dans-bag-lib
    // - add payload files without checksums
    // - calculate missing checksums
    // call the first by the servlet, the latter in this stub
    // the deposit-ui should present missing checksums as ???
  }

  private def finalizeSubmit(stagedSubmitDir: File): Unit = {
    logger.info(s"STUB: Prepare and move to ingest-flow-inbox: $stagedSubmitDir".toString)
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

  override def close(): Unit = watchService.close()
}
