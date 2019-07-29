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

import java.io.IOException
import java.nio.file.StandardWatchEventKinds.{ ENTRY_CREATE, ENTRY_DELETE }
import java.nio.file.{ WatchEvent, WatchKey, WatchService }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

object WorkerThread extends DebugEnhancedLogging {

  // TODO make flexible for unit tests / use EasyDepositApiApp.stagedBaseDir
  val configuration = Configuration(File(System.getProperty("app.home")))
  val apiApp = new EasyDepositApiApp(configuration)

  def main(args: Array[String]): Unit = {

    // TODO create watchService as first action in EasyDepositApiService.start
    //  close it as last action in EasyDepositApiService.stop
    apiApp.stagedBaseDir.watchService.apply { watchService =>
      apiApp.stagedBaseDir.register(watchService, Seq(ENTRY_CREATE, ENTRY_DELETE))
      handleWatchService(watchService) // TODO in a sub-thread created by ServiceStarter.start
    }
  }

  def handleWatchService(watchService: WatchService): Unit = {
    // https://howtodoinjava.com/java8/java-8-watchservice-api-tutorial/
    // used take instead, because polling skips staged-upload-dirs created and deleted within a single cycle
    handleAbortedByHardShutdown() // TODO before starting the thread
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
    val stagedDepositDir = apiApp.stagedBaseDir / event.context.toString
    (event.kind(), getDepositState(stagedDepositDir)) match {
      case (ENTRY_CREATE, Success(State.finalizing)) =>
        logger.info(s"Detected submit event: $stagedDepositDir")
        finalizeSubmit(stagedDepositDir)
      case (ENTRY_DELETE, Success(State.draft)) =>
        logger.info(s"Detected upload event: $stagedDepositDir")
        calculateShas(stagedDepositDir)
      case (_, Success(state)) => logger.warn(s"Not expected deposit state: $state $stagedDepositDir")
      case (_, Failure(e)) => logger.warn(e.getMessage)
      case _ => // either completed finalizeSubmit or started upload
    }
  }

  @throws[IOException]("when files or directories can not be deleted")
  private def handleAbortedByHardShutdown(): Unit = {
    apiApp.stagedBaseDir.list.foreach { abortedDir =>
      getDepositState(abortedDir) match {
        case Success(State.draft) => logger.info(s"Found upload aborted by hard shutdown: $abortedDir")
          // Ignore uploaded files not yet moved into a bag. They may be incomplete and we don't know the target anymore.
          abortedDir.delete() // triggers a running watchService to perform SHA calculations
        case Success(State.finalizing) => logger.info(s"Found submit aborted by hard shutdown: $abortedDir")
          // Don't delete and create the directory again to trigger the watch service.
          // Another hard shutdown could occur in between.
          abortedDir.list.foreach { _.delete() }
          finalizeSubmit(abortedDir)
        case Success(state) => logger.warn(s"Not expected deposit state: $state $abortedDir")
        case Failure(e: Exception) => logger.warn(e.getMessage)
      }
    }
    logger.info(s"So far what was found at startup in ${ apiApp.stagedBaseDir }")
  }

  /**
   * @param stagedDir created with EasyDepositApiApp.getStagedDir
   *                  format: [user-id]-[UUID]-[temp-name]
   */
  private def getDepositState(stagedDir: File) = {
    val name = stagedDir.name.replaceAll("-[^-]+$", "") // trim [temp-name]
    lazy val failure = Failure(new Exception(s"Expecting [user-id]-[UUID]-[temp-name], got $stagedDir"))
    name.indexOf('-') match {
      case -1 => failure
      case i => Try { UUID.fromString(name.substring(i + 1)) }
        .recoverWith { case _ => failure }
        .flatMap(apiApp.getDepositState(name.substring(0, i), _))
        .map(_.state)
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
