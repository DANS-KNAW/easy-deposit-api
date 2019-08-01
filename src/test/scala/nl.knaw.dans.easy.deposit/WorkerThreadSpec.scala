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

import java.util.UUID
import java.util.UUID.randomUUID

import better.files.File
import com.typesafe.scalalogging.Logger
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory
import org.slf4j.{ Logger => Underlying }

class WorkerThreadSpec extends TestSupportFixture with MockFactory with DebugEnhancedLogging {

  // TODO replace manual checks with log interception and verification
  //  test/resource/logback.xml :
  //     <pattern>%d{mm:ss.SSS} [%thread] %-5level %msg%n</pattern>
  //     <logger name="nl.knaw.dans.easy" level="trace" />

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    System.setProperty("app.home", testDir.toString())
    val config = minimalAppConfig
    config.properties.save(((testDir / "cfg").createDirectories() / "application.properties").toJava)
    ((testDir / "bin").createDirectories() / "version").write(config.version)
  }

  "handleAbortedByHardShutdown" should "finish aborted tasks" in {

    implicit val apiApp: EasyDepositApiApp = createApiApp
    val watchedDir = apiApp.stagedBaseDir.createDirectories()
    (watchedDir / s"foo-${randomUUID().toString.replaceAll("-","X")}-4").createDirectories()
    (watchedDir / s"bar-${ randomUUID() }-5").createDirectories()
    (watchedDir / s"somebody-${ createDraftDeposit("somebody") }-6").createDirectories()
    (watchedDir / s"someone-${ createFinalizingDeposit("someone") }-7").createDirectories()

    val mockedLogger = mock[Underlying] // TODO needed s"...".toString to avoid not expected WrappedArray
    (mockedLogger.warn(_: String)) expects where { s: String => isInvalidDir(s, "staged/foo-") } once()
    (mockedLogger.warn(_: String)) expects where { s: String => s.contains("NoSuchDepositException") && s.contains("staged/bar-") } once()
    (mockedLogger.info(_: String)) expects where { s: String => s.startsWith("STUB: Calculate")&& s.contains("staged/somebody-") } once()
    (mockedLogger.info(_: String)) expects where { s: String => s.startsWith("STUB: Prepare")&& s.contains("staged/someone-") } once()
    notSignificantLogExpectations(mockedLogger)

    val thread = new WorkerThread(apiApp) {
      override protected lazy val logger: Logger = Logger(mockedLogger)
    }
    thread.start()
    Thread.sleep(10000) // TODO is this some file system dependent delay to pick up the delete event for SHA calculation?
  }

  private def isInvalidDir(s: String, part: String) = {
    s.endsWith("InvalidStagedDirException: Expecting a directory [user-id]-[UUID]-[temp-name])") &&
      s.contains(part)
  }

  "handleWatchEvent" should "execute both types of actions" in {

    implicit val apiApp: EasyDepositApiApp = createApiApp
    val watchedDir = apiApp.stagedBaseDir.createDirectories()

    val mockedLogger = mock[Underlying] // TODO needed s"...".toString in WorkerThread to avoid not expected WrappedArray
    (mockedLogger.info(_: String)) expects where { s: String => s.startsWith("STUB: Calculate")&& s.contains("staged/somebody-") } once()
    (mockedLogger.info(_: String)) expects where { s: String => s.startsWith("STUB: Prepare")&& s.contains("staged/someone-") } once()
    notSignificantLogExpectations(mockedLogger)

    val thread = new WorkerThread(apiApp) {
      override protected lazy val logger: Logger = Logger(mockedLogger)
      setUncaughtExceptionHandler( // TODO terminate api when worker-thread terminates unexpectedly
        (t: Thread, e: Throwable) =>
          throw new Exception(s"${ t.getId } threw", e)
      )
    }
    // TODO enable manual test on deasy: implement start thread as first action in EasyDepositApiService.start
    //  would we also need to wait 5 seconds between all create/delete actions?
    thread.start()
    Thread.sleep(5000)
    val dir = (watchedDir / s"somebody-${ createDraftDeposit("somebody") }-6").createDirectories()
    (dir / "dummy").createFile().exists
    Thread.sleep(5000)
    dir.delete()
    Thread.sleep(5000)
    (watchedDir / s"someone-${ createFinalizingDeposit("someone") }-7").createDirectories()
    Thread.sleep(5000)
  }

  private def createApiApp: EasyDepositApiApp = {

    val configuration = Configuration(File(System.getProperty("app.home")))
    new EasyDepositApiApp(configuration)

  }

  private def createFinalizingDeposit(user: String)(implicit apiApp: EasyDepositApiApp): UUID = {
    val uuid = createDraftDeposit(user)
    val depositProps = new PropertiesConfiguration(
      (testDir / apiApp.properties.getString("deposits.drafts") / user / uuid.toString / "deposit.properties").toJava
    )
    depositProps.setProperty("state.label", "FINALIZING")
    depositProps.save()
    uuid
  }

  private def createDraftDeposit(user: String)(implicit apiApp: EasyDepositApiApp): UUID = {
    apiApp.createDeposit(user).get.id // TODO unsafe get
  }

  private def notSignificantLogExpectations(mockedLogger: Underlying): Unit = {
    (() => mockedLogger.isInfoEnabled()) expects() anyNumberOfTimes() returning true
    (() => mockedLogger.isWarnEnabled()) expects() anyNumberOfTimes() returning true
    (mockedLogger.info(_: String)) expects * anyNumberOfTimes()
    (mockedLogger.warn(_: String)) expects * anyNumberOfTimes()
    (mockedLogger.warn(_: String, _: Throwable)) expects(*, *) anyNumberOfTimes()
  }
}
