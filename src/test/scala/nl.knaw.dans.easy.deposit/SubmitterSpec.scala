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
import java.nio.file.{ NoSuchFileException, Paths }
import java.util.UUID

import better.files.{ File, StringExtensions }
import javax.activation.DataSource
import nl.knaw.dans.easy.deposit.Errors.{ CorruptDepositException, InvalidDocumentException }
import nl.knaw.dans.easy.deposit.docs.{ AgreementData, DatasetMetadata }
import nl.knaw.dans.easy.deposit.executor.{ JobQueueManager, SystemStatus }
import nl.knaw.dans.lib.error._
import org.apache.commons.mail.MultiPartEmail
import org.scalamock.scalatest.MockFactory
import scalaj.http.Http

import scala.util.{ Failure, Success, Try }

class SubmitterSpec extends TestSupportFixture with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val submitDir: File = testDir / "submitted"
  private val stageDir: File = testDir / "staged"
  private val depositHome = "https://easy.dans.knaw.nl/deposit"
  private val easyHome = new URL("https://easy.dans.knaw.nl/ui")
  private val validGroup = submitDir.fileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(userGroup)
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
    submitDir.createDirectories()
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
    val (draftDeposit, draftProperties) = init(withDoiInProps = true)

    // pre conditions
    draftProperties.contentAsString should include("DRAFT")
    draftProperties.contentAsString shouldNot include("bag-store.bag-id")

    new Submitter(submitDir, validGroup, depositHome, jobQueue = expectsOneSubmitJob(), mailer = null, agreementGenerator = null)
      .submit(draftDeposit, createStateManager(draftDeposit), defaultUserInfo, stageDir) shouldBe a[Success[_]]

    // post conditions
    draftProperties.contentAsString should include("SUBMITTED")
    draftProperties.contentAsString should include("bag-store.bag-id")
  }

  it should "catch incomplete metatdata" in {
    val (draftDeposit, _) = init(DatasetMetadata())

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage.startsWith("invalid DatasetMetadata: ") =>
    }
  }

  it should "catch missing properties" in {
    val (draftDeposit, _) = init()
    (draftDeposit.bagDir.parent / "deposit.properties").delete()

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should matchPattern {
      case Failure(e: CorruptDepositException) if e.getMessage.contains("deposit.properties not found or empty") =>
    }
  }

  it should "catch a missing bag" in {
    val (draftDeposit, _) = init(withDoiInProps = true)
    draftDeposit.bagDir.delete()

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should matchPattern {
      case Failure(e: NoSuchFileException) if e.getMessage.contains("bag/bagit.txt") =>
    }
  }

  it should "catch inconsistency between actual payloads and manifest" in {
    val (draftDeposit, _) = init(withDoiInProps = true)
    val file = (draftDeposit.bagDir / "data" / "some.file").createFile()

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, createStateManager(draftDeposit), defaultUserInfo, file) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        s"invalid bag, missing [files, checksums]: [Set(), Set($file)]" =>
    }
  }

  "SubmitJob.run" should "write all files" in {
    val customMessage = "Lorum ipsum"
    val (draftDeposit, draftProperties) = init(
      datasetMetadata.copy(messageForDataManager = Some(customMessage)),
      withDoiInProps = true
    )
    val bag = draftDeposit.getDataFiles.getOrRecover(e => fail(e.toString, e)).bag
    bag.addPayloadFile("Lorum ipsum".inputStream, Paths.get("folder/text.txt"))
    bag.save()

    // pre conditions
    draftProperties.contentAsString should
      (include("DRAFT") and not include "bag-store.bag-id")
    val jsonFile = draftDeposit.bagDir / "metadata" / "dataset.json"
    val mdOldSize = jsonFile.size // should not change
    stageDir should not(exist)
    submitDir.list shouldBe empty

    new Submitter(submitDir, validGroup, depositHome, jobQueue = executesOneSubmitJob(), mailer = expectsBuildMessageOnce(), agreementGenerator = expectsOneAgreement())
      .submit(draftDeposit, createStateManager(draftDeposit), defaultUserInfo, stageDir) shouldBe Success(())

    // post conditions
    jsonFile.size shouldBe mdOldSize
    val submittedDeposit = submitDir.children.toSeq.head
    draftProperties.contentAsString should // compare with pre conditions
      (include("SUBMITTED") and include("bag-store.bag-id") and be((submittedDeposit / "deposit.properties").contentAsString))
    val submittedMetadata = submittedDeposit / "bag" / "metadata"
    (submittedMetadata / "files.xml").contentAsString should include("""<file filepath="data/folder/text.txt">""")
    (submittedMetadata / "depositor-info" / "message-from-depositor.txt").contentAsString shouldBe
      s"""$customMessage
         |
         |The deposit can be found at $depositHome/${ draftDeposit.id }""".stripMargin
    (draftDeposit.bagDir / "metadata").list.toSeq.map(_.name) shouldBe Seq("dataset.json") // compare with submitted metadata
    submittedDeposit.listRecursively.toSeq should contain allElementsOf Seq(
      "deposit.properties",
      "bag",
      "bag/bagit.txt",
      "bag/bag-info.txt",
      "bag/manifest-sha1.txt",
      "bag/tagmanifest-sha1.txt",
      "bag/data",
      "bag/data/folder",
      "bag/data/folder/text.txt",
      "bag/metadata",
      "bag/metadata/dataset.xml",
      "bag/metadata/files.xml",
      "bag/metadata/depositor-info",
      "bag/metadata/depositor-info/agreements.xml",
      "bag/metadata/depositor-info/message-from-depositor.txt",
    ).map(submittedDeposit / _)
  }

  /** use null instead when not expecting to arrive at requesting/changing any state */
  private def createStateManager(draftDeposit: DepositDir) = {
    draftDeposit.getStateManager(submitDir, easyHome)
      .getOrRecover(e => fail(s"could not get stateManager of test deposit $e"))
  }

  private def executesOneSubmitJob(): JobQueueManager = {
    val mocked = mock[JobQueueManager]
    (() => mocked.getSystemStatus) expects() once() returning
      SystemStatus(threadPoolStatus = null, queueSize = 2, queueContent = null)
    ((mocked.scheduleJob(_: SubmitJob)) expects *).onCall { job: SubmitJob => Try(job.run()) } once()
    mocked
  }

  /** use null instead when not expecting to put a job on the queue */
  private def expectsOneSubmitJob(expectedResult: Try[Unit] = Success(())): JobQueueManager = {
    val mocked = mock[JobQueueManager]
    (() => mocked.getSystemStatus) expects() once() returning
      SystemStatus(threadPoolStatus = null, queueSize = 2, queueContent = null)
    (mocked.scheduleJob(_: Runnable)) expects * once() returning expectedResult
    mocked
  }

  /** use null instead when not expecting to generate an agreement */
  private def expectsOneAgreement(expectedResult: Try[Array[Byte]] = Success("".getBytes)): AgreementGenerator = {
    class Mocked() extends AgreementGenerator(Http, new URL("http://does.not.exist"), acceptHeader = "application/pdf")
    val mocked = mock[Mocked]
    (mocked.generate(_: AgreementData, _: UUID)
      ) expects(*, *) once() returning expectedResult
    mocked
  }

  /** use null instead when not expecting to compose an email */
  private def expectsBuildMessageOnce(expectedResult: Try[MultiPartEmail] = Success(expectsSendOnce())): Mailer = {
    class Mocked extends Mailer(
      smtpHost = "",
      fromAddress = "",
      bounceAddress = "",
      bccs = Seq.empty,
      templateDir = File("src/main/assembly/dist/cfg/template"),
      myDatasets = new URL(depositHome)
    )
    val mocked = mock[Mocked]
    (mocked.buildMessage(_: AgreementData, _: Map[String, DataSource], _: UUID, _: Option[String])
      ) expects(*, *, *, *) once() returning expectedResult
    mocked
  }

  private def expectsSendOnce (expectedResult: String = "123"): MultiPartEmail = {
    val mocked = mock[MultiPartEmail]
    (() => mocked.sendMimeMessage()) expects() once() returning expectedResult
    mocked
  }
}
