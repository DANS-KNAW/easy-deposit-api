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
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.GroupPrincipal
import java.nio.file.{ NoSuchFileException, Paths }
import java.util.concurrent.{ Executor, TimeUnit }

import better.files.{ File, StringExtensions }
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.easy.deposit.Errors.{ CorruptDepositException, InvalidDocumentException }
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, StateInfo }
import nl.knaw.dans.easy.deposit.executor.{ JobQueueManager, SystemStatus }
import nl.knaw.dans.lib.error._
import okhttp3.mockwebserver.{ MockResponse, MockWebServer }
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ Assertion, BeforeAndAfterAll }
import scalaj.http.Http

import scala.util.{ Failure, Success, Try }

class SubmitterSpec extends TestSupportFixture with MockFactory with BeforeAndAfterAll {

  case class MailExecutor() extends Executor {override def execute(command: Runnable): Unit = () }
  override protected def beforeAll(): Unit = {
    // TODO next line not effective for https://github.com/apache/commons-email/blob/3fd73a641c3f729586824da1c17aa8efd0e895f8/src/main/java/org/apache/commons/mail/Email.java#L637
    //  see also https://github.com/m-szalik/javamail/blob/master/docs/examples/standalone-example/src/main/java/org/jsoftware/javamail/MainAppExample.java
    //  which would write the emails to files but doesn't seem to collaborate with apache.commons.mail
    //  Note that the required session is set by buildMimeMessage in the Mailer class.
    //  As a result sendMimeMessage in SubmitJob.run throws the non-fatal
    //  "EmailException: Sending the email to the following server failed mail.server.does.not.exist.dans.knaw.nl"
    System.setProperty("mail.event.executor", "nl.knaw.dans.easy.deposit.SubmitterSpec$MailExecutor$")
    super.beforeAll()
  }

  // configure the mock server
  private val server = new MockWebServer
  private val agreementTestServer = "/generate/"
  private val contentType = "application/pdf"
  private val agreementGenerator = AgreementGenerator(Http, server.url(agreementTestServer).url(), contentType)

  override protected def afterAll(): Unit = {
    server.shutdown()
    super.afterAll()
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val submitDir: File = testDir / "submitted"
  private val stageDir: File = testDir / "staged"
  private val depositHome = "https://easy.dans.knaw.nl/deposit"
  private val easyHome = new URL("https://easy.dans.knaw.nl/ui")
  private val validGroup = principalOf(userGroup)
  private val datasetMetadata: DatasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
    .getOrRecover(e => fail("could not get test input", e))
    .copy(messageForDataManager = Some("Lorum Ipsum")) // for a short should be check

  def prepareTestDeposit(datasetMetadata: DatasetMetadata = datasetMetadata,
                         withDoiInProps: Boolean = false,
                        ): (DepositDir, File, DansBag, String) = {

    val deposit = DepositDir.create(testDir / "drafts", "user")
      .getOrRecover(e => fail(e.toString, e))

    val bag = deposit.getDataFiles.getOrRecover(e => fail("can't get bag of test input", e)).bag

    val props = bag.baseDir.parent / "deposit.properties"
    val doi: String = datasetMetadata.doi.getOrElse("")
    if (withDoiInProps) {
      if (doi.isEmpty) fail(new IllegalArgumentException("can't add to to props when metadata does not have one"))
      props.append(s"identifier.doi=${ doi }")
    }

    deposit.writeDatasetMetadataJson(datasetMetadata)
    submitDir.createDirectories()

    (deposit, props, bag, doi)
  }

  "constructor" should "fail if the configured group does not exist" in {
    val props = minimalAppConfig.properties
    props.setProperty("deposit.permissions.group", "not-existing-group")

    // the App creates the Submitter
    the[IOException] thrownBy new EasyDepositApiApp(new Configuration("", props)) should
      have message "Group not-existing-group could not be found"
  }

  "submit" should "schedule a job" in {
    val (draftDeposit, draftProperties, _, _) = prepareTestDeposit(withDoiInProps = true)

    // pre conditions
    draftProperties.contentAsString should (include("DRAFT") and not include "bag-store.bag-id")

    new Submitter(
      submitDir, validGroup, depositHome, mailer = null, agreementGenerator = null,
      jobQueue = expectsScheduleJob(onSchedule = _ => Success(())),
    ).submit(draftDeposit, stateManagerOf(draftDeposit), defaultUserInfo, stageDir) shouldBe a[Success[_]]

    // post conditions
    draftProperties.contentAsString should (include("SUBMITTED") and include("bag-store.bag-id"))
  }

  it should "catch incomplete metatdata" in {
    val (draftDeposit, _, _, _) = prepareTestDeposit(DatasetMetadata())

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage.startsWith("invalid DatasetMetadata: ") =>
    }
  }

  it should "catch missing properties" in {
    val (draftDeposit, _, _, _) = prepareTestDeposit()
    (draftDeposit.bagDir.parent / "deposit.properties").delete()

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should
      matchPattern { case Failure(e: CorruptDepositException) if e.getMessage ==
        s"Invalid deposit uuid ${ draftDeposit.id } for user user: deposit.properties not found or empty" =>
      }
  }

  it should "catch a missing bag" in {
    val (draftDeposit, _, bag, _) = prepareTestDeposit(withDoiInProps = true)
    draftDeposit.bagDir.delete()

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, draftDepositStateManager = null, defaultUserInfo, stageDir) should
      matchPattern { case Failure(e: NoSuchFileException) if e.getMessage == s"${ bag.baseDir }/bagit.txt" =>
      }
  }

  it should "catch inconsistencies between actual payloads and manifest" in {
    val (draftDeposit, _, _, _) = prepareTestDeposit(withDoiInProps = true)
    val someFile = (draftDeposit.bagDir / "data" / "some.file").createFile()
    val otherFile = someFile.parent.parent / "other.file"
    (draftDeposit.bagDir / "manifest-sha1.txt").append(s"cheksum123 ${ otherFile.name }")

    new Submitter(submitDir, validGroup, depositHome, jobQueue = null, mailer = null, agreementGenerator = null)
      .submit(draftDeposit, stateManagerOf(draftDeposit), defaultUserInfo, someFile) should
      matchPattern { case Failure(e: Exception) if e.getMessage ==
        s"invalid bag, missing [files, checksums]: [Set($otherFile), Set($someFile)]" =>
      }
  }

  "SubmitJob.run" should "write all files" in {
    // assemble test data
    val (draftDeposit, draftPropertiesFile, draftBag, doi) = prepareTestDeposit(withDoiInProps = true)
    draftBag.addPayloadFile("Lorum ipsum".inputStream, Paths.get("folder/text.txt"))
    draftBag.save()

    // pre conditions
    draftPropertiesFile.contentAsString should
      (include("DRAFT") and not include "bag-store.bag-id")
    val jsonFile = draftDeposit.bagDir / "metadata" / "dataset.json"
    val mdOldSize = jsonFile.size // should not change
    stageDir should not(exist)
    submitDir.list shouldBe empty

    server.enqueue { pdfResponse }
    new Submitter(submitDir, validGroup, depositHome, jobQueue = expectsScheduleJob(), createMailer, agreementGenerator)
      .submit(draftDeposit, stateManagerOf(draftDeposit), defaultUserInfo, stageDir) shouldBe Success(())
    takePdfAgreementRequestFromServer(doi)

    // post conditions
    val submittedDeposit = submitDir / load(draftPropertiesFile).getString("bag-store.bag-id")
    draftPropertiesFile.contentAsString should // compare with pre conditions
      (include("SUBMITTED") and include("bag-store.bag-id") and be((submittedDeposit / "deposit.properties").contentAsString))
    jsonFile.size shouldBe mdOldSize
    stateManagerOf(draftDeposit).getStateInfo shouldBe Success(StateInfo(State.submitted, "The deposit is being processed"))
    (draftDeposit.bagDir / "metadata").list.toSeq.map(_.name) shouldBe Seq("dataset.json") // compare with submitted bag/metadata files
    val submittedMetadata = submittedDeposit / "bag" / "metadata"
    submittedDeposit.listRecursively.toSeq.map(_.toString) should contain theSameElementsAs Seq(
      s"$submittedDeposit/deposit.properties",
      s"$submittedDeposit/bag",
      s"$submittedDeposit/bag/bagit.txt",
      s"$submittedDeposit/bag/bag-info.txt",
      s"$submittedDeposit/bag/manifest-sha1.txt",
      s"$submittedDeposit/bag/tagmanifest-sha1.txt",
      s"$submittedDeposit/bag/data",
      s"$submittedDeposit/bag/data/folder",
      s"$submittedDeposit/bag/data/folder/text.txt",
      s"$submittedMetadata",
      s"$submittedMetadata/dataset.xml",
      s"$submittedMetadata/files.xml",
      s"$submittedMetadata/depositor-info",
      s"$submittedMetadata/depositor-info/agreements.xml",
      s"$submittedMetadata/depositor-info/message-from-depositor.txt",
    )
    (submittedMetadata / "files.xml").contentAsString should include("""<file filepath="data/folder/text.txt">""")
    (submittedMetadata / "depositor-info" / "message-from-depositor.txt").contentAsString shouldBe
      s"""Lorum Ipsum
         |
         |The deposit can be found at $depositHome/${ draftDeposit.id }""".stripMargin
  }

  it should "write empty message-from-depositor file" in {
    // prepare test data
    val (draftDeposit, draftPropertiesFile, _, doi) = prepareTestDeposit(
      datasetMetadata.copy(messageForDataManager = None),
      withDoiInProps = true
    )

    server.enqueue { pdfResponse }
    new Submitter(submitDir, validGroup, depositHome, jobQueue = expectsScheduleJob(), createMailer, agreementGenerator)
      .submit(draftDeposit, stateManagerOf(draftDeposit), defaultUserInfo, stageDir) shouldBe Success(())
    takePdfAgreementRequestFromServer(doi)

    // post condition (other details in previous test)
    val uuid = load(draftPropertiesFile).getString("bag-store.bag-id")
    (submitDir / uuid / "bag" / "metadata" / "depositor-info" / "message-from-depositor.txt").contentAsString shouldBe
      s"The deposit can be found at $depositHome/${ draftDeposit.id }"
  }

  it should "replace a previous property bag-store.bag-id at resubmit" in {
    val (draftDeposit, draftPropertiesFile, _, doi) = prepareTestDeposit(withDoiInProps = true)
    val submitter = new Submitter(submitDir, validGroup, depositHome, jobQueue = expectsScheduleJob(times = 2), createMailer, agreementGenerator)

    def submit = {
      server.enqueue { pdfResponse }
      submitter.submit(draftDeposit, stateManagerOf(draftDeposit), defaultUserInfo, stageDir) shouldBe Success(())
      takePdfAgreementRequestFromServer(doi)
    }

    def currentBagStoreBagId: String = { // the result changes by each submit
      load(draftPropertiesFile).getString("bag-store.bag-id")
    }

    def mimicRejectAndSaveDraft = {
      draftPropertiesFile.write(draftPropertiesFile.contentAsString.replace("SUBMITTED", "DRAFT"))
    }

    currentBagStoreBagId shouldBe null // pre condition

    // tested scenario
    submit
    mimicRejectAndSaveDraft
    val rejectedId = currentBagStoreBagId // intermediate post condition
    submit
    val resubmittedId = currentBagStoreBagId // final post condition

    // Q.E.D.
    submitDir.list.toSeq.map(_.name) should contain theSameElementsAs Seq(rejectedId, resubmittedId)
    readSubmittedBag(rejectedId) should contain theSameElementsAs readSubmittedBag(resubmittedId)
    // the deposit.properties files will have different values for the bag-store.bag-id
  }

  it should "report an inconsistent checksum" in {
    val (draftDeposit, _, _, _) = prepareTestDeposit(withDoiInProps = true)
    val bag = draftDeposit.getDataFiles.getOrRecover(e => fail("can't get bag of test input", e)).bag
    bag.addPayloadFile("lorum ipsum".inputStream, Paths.get("file.txt"))
    bag.save()
    val manifest = draftDeposit.bagDir / "manifest-sha1.txt"
    manifest.write(manifest.contentAsString.replaceAll(" +", "xxx  "))

    new Submitter(submitDir, validGroup, depositHome, jobQueue = expectsScheduleJob(), createMailer, agreementGenerator)
      .submit(draftDeposit, stateManagerOf(draftDeposit), defaultUserInfo, stageDir) shouldBe Success(())

    stageDir.list shouldNot be(empty)
    submitDir.list shouldBe empty
    stateInfoShouldHaveContactDansMessage(stateManagerOf(draftDeposit))
  }

  it should "report a group configuration problem" in {
    val (draftDeposit, _, _, _) = prepareTestDeposit(withDoiInProps = true)
    val stateManager = stateManagerOf(draftDeposit)

    new Submitter(submitDir, principalOf(unrelatedGroup), depositHome, jobQueue = expectsScheduleJob(), createMailer, agreementGenerator)
      .submit(draftDeposit, stateManager, defaultUserInfo, stageDir) shouldBe Success(())

    // post conditions
    stageDir.list shouldNot be(empty)
    submitDir.list shouldBe empty
    stateInfoShouldHaveContactDansMessage(stateManager)
  }

  it should "report an unexpected exception" in {
    val (draftDeposit, draftPropertiesFile, _, _) = prepareTestDeposit(withDoiInProps = true)
    val groupPrincipal = new GroupPrincipal() {override def getName: String = "invalidGroupPrincipal" } // causes ProviderMismatchException

    new Submitter(submitDir, groupPrincipal, depositHome, jobQueue = expectsScheduleJob(), createMailer, agreementGenerator)
      .submit(draftDeposit, stateManagerOf(draftDeposit), defaultUserInfo, stageDir) shouldBe Success(())

    // post conditions
    stageDir.list shouldNot be(empty)
    submitDir.list shouldBe empty
    stateInfoShouldHaveContactDansMessage(stateManagerOf(draftDeposit))
    draftPropertiesFile.contentAsString should include("bag-store.bag-id = ")
  }

  private def readSubmittedBag(id: String): Seq[String] = {
    (submitDir / id / "bag").listRecursively.toSeq
      .withFilter(!_.isDirectory)
      .map(_.contentAsString)
  }

  private def stateInfoShouldHaveContactDansMessage(stateManager: StateManager): Assertion = {
    stateInfo(stateManager) should matchPattern {
      case StateInfo(State.submitted, msg) if msg.startsWith("Something went wrong while processing this deposit. Please ") =>
    }
  }

  private def load(file: File) = new PropertiesConfiguration(file.toJava)

  private def principalOf(group: String) = {
    submitDir.fileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(group)
  }

  private def stateInfo(stateManager: StateManager) = {
    stateManager.getStateInfo.getOrRecover(e => fail("could not get stateInfo of tested draft deposit", e))
  }

  private def okResponse = new MockResponse().setResponseCode(200)

  private def pdfResponse = okResponse.addHeader("Content-Type", contentType).setBody("mocked pdf")

  def takePdfAgreementRequestFromServer(doi: String): Assertion = {
    val request = Option(server.takeRequest(1, TimeUnit.SECONDS))
      .getOrElse(fail("expected pdf agreement request did not happen"))
    request.getMethod shouldBe "POST"
    request.getPath shouldBe "/generate/"
    request.getHeader("accept") shouldBe contentType
    request.getBody.readString(StandardCharsets.UTF_8) shouldBe
      s"""{"depositor":{"name":"fullName","address":"","zipcode":"","city":"","country":"","organisation":"","phone":"","email":"does.not.exist@dans.knaw.nl"},"doi":"$doi","title":"title 1","dateSubmitted":"2018-03-22","dateAvailable":"2018-03-14","accessCategory":"OPEN_ACCESS","license":"http://creativecommons.org/publicdomain/zero/1.0","sample":false,"agreementVersion":"4.0","agreementLanguage":"EN"}"""
  }

  /**
   * Values are cashed so create a fresh one each time to be sure the value was flushed to the file system.
   * After all, each service request also provides a fresh instance.
   * Use null instead when not expecting to arrive at requesting/changing any state.
   **/
  private def stateManagerOf(draftDeposit: DepositDir) = {
    draftDeposit.getStateManager(submitDir, easyHome)
      .getOrRecover(e => fail(s"could not get stateManager of test deposit $e"))
  }

  private def expectsScheduleJob(onSchedule: SubmitJob => Try[Unit] = (job: SubmitJob) => Try(job.run()),
                                 times: Int = 1,
                                ): JobQueueManager = {
    val mocked = mock[JobQueueManager]

    // only called when log-level is info
    (() => mocked.getSystemStatus) expects() anyNumberOfTimes() returning
      SystemStatus(threadPoolStatus = null, queueSize = 2, queueContent = null)

    ((mocked.scheduleJob(_: SubmitJob)) expects *)
      .onCall { job: SubmitJob => onSchedule(job) } repeat times

    mocked
  }

  /** Use null instead when not expecting to compose an email. */
  private def createMailer: Mailer = {
    new Mailer(
      smtpHost = "mail.server.does.not.exist.dans.knaw.nl",
      fromAddress = "from.does.not.exist@dans.knaw.nl",
      bounceAddress = "bounce.does.not.exist@dans.knaw.nl",
      bccs = Seq("bcc.does.not.exist@dans.knaw.nl"),
      templateDir = File("src/main/assembly/dist/cfg/template"),
      myDatasets = new URL(depositHome)
    )
  }
}
