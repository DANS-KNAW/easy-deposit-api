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
import java.net.URL
import java.nio.file.NoSuchFileException

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.{ CorruptDepositException, InvalidDocumentException }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata
import nl.knaw.dans.easy.deposit.executor.{ JobQueueManager, SystemStatus }
import nl.knaw.dans.lib.error._
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success }

class SubmitterSpec extends TestSupportFixture with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val submitDir: File = testDir / "submitted"
  private val stageDir: File = testDir / "staged"
  private val datasetMetadata: DatasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
    .getOrRecover(e => fail("could not get test input", e))

  def init(datasetMetadata: DatasetMetadata = datasetMetadata,
           withDoiInProps: Boolean = false,
          ): (DepositDir, File) = {

    val deposit = DepositDir.create(testDir / "drafts", "user")
      .getOrRecover(e => fail(e.toString, e))

    deposit.writeDatasetMetadataJson(datasetMetadata)

    val bag = deposit.getDataFiles
      .getOrRecover(e => fail(e.toString, e)).bag

    val props = bag.baseDir.parent / "deposit.properties"
    if (withDoiInProps) {
      val doi: String = datasetMetadata.doi
        .getOrElse(fail("could not get DOI from test input"))
      props.append(s"identifier.doi=${ doi }")
    }
    deposit -> props
  }

  "constructor" should "fail if the configured group does not exist" in {
    val props = minimalAppConfig.properties
    props.setProperty("deposit.permissions.group", "not-existing-group")

    // the App creates the Submitter
    the[IOException] thrownBy new EasyDepositApiApp(new Configuration("", props)) should
      have message "Group not-existing-group could not be found"
  }

  "submit" should "schedule a job" in {
    val (draftDeposit, draftPropertiess) = init(withDoiInProps = true)

    val mockedQueue = mock[JobQueueManager]
    (() => mockedQueue.getSystemStatus) expects() returning
      SystemStatus(threadPoolStatus = null, queueSize = 2, queueContent = null)
    (mockedQueue.scheduleJob(_: Runnable)) expects * returning Success(())

    // pre conditions
    draftPropertiess.contentAsString should include("DRAFT")
    draftPropertiess.contentAsString shouldNot include("bag-store.bag-id")

    createSubmitter(mockedQueue)
      .submit(draftDeposit, createStateManager(draftDeposit), defaultUserInfo, stageDir) shouldBe a[Success[_]]

    // post conditions
    draftPropertiess.contentAsString should include("SUBMITTED")
    draftPropertiess.contentAsString should include("bag-store.bag-id")
  }

  it should "catch incomplete metatdata" in {
    val (draftDeposit, _) = init(DatasetMetadata())

    createSubmitter()
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage.startsWith("invalid DatasetMetadata: ") =>
    }
  }

  it should "catch missing properties" in {
    val (draftDeposit, _) = init()
    (draftDeposit.bagDir.parent / "deposit.properties").delete()

    createSubmitter()
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should matchPattern {
      case Failure(e: CorruptDepositException) if e.getMessage.contains("deposit.properties not found or empty") =>
    }
  }

  it should "catch a missing bag" in {
    val (draftDeposit, _) = init(withDoiInProps = true)
    draftDeposit.bagDir.delete()

    createSubmitter()
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should matchPattern {
      case Failure(e: NoSuchFileException) if e.getMessage.contains(s"$bagDirName/bagit.txt") =>
    }
  }

  it should "catch inconsistency between actual payloads and manifest" in {
    val (draftDeposit, _) = init(withDoiInProps = true)
    val file = (draftDeposit.bagDir / "data" / "some.file").createFile()

    createSubmitter()
      .submit(draftDeposit, createStateManager(draftDeposit), defaultUserInfo, file) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        s"invalid bag, missing [files, checksums]: [Set(), Set($file)]" =>
    }
  }

  /** use null instead when not expecting to arrive at requesting/changing any state */
  private def createStateManager(draftDeposit: DepositDir) = {
    draftDeposit.getStateManager(submitDir, new URL("https://easy.dans.knaw.nl/ui"))
      .getOrRecover(e => fail(s"could not get stateManager of test deposit $e"))
  }

  /** @param jobQueue use a mock when specified */
  private def createSubmitter(jobQueue: JobQueueManager = null) = {
    // the null values are only passed on to jobQueue.submitJob(new SubmitJob(...))
    new Submitter(
      submitToBaseDir = submitDir,
      groupPrincipal = null,
      depositUiURL = null,
      jobQueue = jobQueue,
      mailer = null,
      agreementGenerator = null,
    )
  }
}
