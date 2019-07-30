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
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory

class WorkerThreadSpec extends TestSupportFixture with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    System.setProperty("app.home", testDir.toString())
    val config = minimalAppConfig
    config.properties.save(((testDir / "cfg").createDirectories() / "application.properties").toJava)
    ((testDir / "bin").createDirectories() / "version").write(config.version)
  }

  // TODO replace manual check with log interception and verification
  //  test/resource/logback.xml : <logger name="nl.knaw.dans.easy" level="trace" />
  "WorkerThread" should "log various actions" in {

    implicit val apiApp: EasyDepositApiApp = {
      val configuration = Configuration(File(System.getProperty("app.home")))
      new EasyDepositApiApp(configuration)
    }
    val watchedDir = apiApp.stagedBaseDir.createDirectories()
    (watchedDir / "garbage").createDirectories()
    (watchedDir / "foo-123-4").createDirectories()
    (watchedDir / s"bar-${ randomUUID() }-5").createDirectories()
    (watchedDir / s"somebody-${ createDraftDeposit("somebody") }-6").createDirectories()
    (watchedDir / s"someone-${ createFinalizingDeposit("someone") }-7").createDirectories()

    val thread = WorkerThread(apiApp)
    // TODO deamon? setUncaughtExceptionHandler?
    //  when/how to terminate allowing to complete pending events
    thread.start()
    println(s"${ thread.getState } ")
    // while (thread.getState != State.WAITING) Thread.sleep(50)
    Thread.sleep(5000)
    println(s"${ thread.getState } ")
    val dir = (watchedDir / s"somebody2-${ createDraftDeposit("somebody2") }-6").createDirectories()
    Thread.sleep(5000)
    dir.delete()
    Thread.sleep(5000)
    (watchedDir / s"someone2-${ createFinalizingDeposit("someone2") }-7").createDirectories()
    Thread.sleep(5000)
  }

  private def createFinalizingDeposit(user: String)(implicit apiApp: EasyDepositApiApp): UUID = {
    val uuid = createDraftDeposit(user)
    val depositProps = new PropertiesConfiguration(
      (testDir / apiApp.properties.getString("deposits.drafts") / user / uuid.toString / "deposit.properties").toJava
    )
    depositProps.setProperty("state.label", "FINALIZING") // TODO not yet implemented in current StateManager
    depositProps.save()
    uuid
  }

  private def createDraftDeposit(someone: String)(implicit apiApp: EasyDepositApiApp): UUID = {
    apiApp.createDeposit(someone).get.id // TODO unsafe get
  }
}
