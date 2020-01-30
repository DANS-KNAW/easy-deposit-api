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
package nl.knaw.dans.easy.deposit.servlets

import java.util.UUID

import nl.knaw.dans.easy.deposit._
import nl.knaw.dans.easy.deposit.authentication.{ AuthenticationMocker, AuthenticationProvider }
import nl.knaw.dans.easy.deposit.docs._
import nl.knaw.dans.lib.error._
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Success

class IntegrationSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  private val authMocker = new AuthenticationMocker() {
    override val mockedAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val app: EasyDepositApiApp = createTestApp()
  mountServlets(app, authMocker.mockedAuthenticationProvider)

  "scenario: /deposit/:uuid/metadata life cycle" should "return default dataset metadata" in {
    authMocker.expectsUserFooBar
    val uuid = createDeposit
    val metadataURI = s"/deposit/$uuid/metadata"

    // create dataset metadata
    authMocker.expectsUserFooBar
    put(
      metadataURI, headers = Seq(fooBarBasicAuthHeader),
      body = """{"titles":["blabla"]}""" // more variations in DepositDirSpec
    ) {
      status shouldBe NO_CONTENT_204
    }
    (testDir / "drafts" / "foo" / uuid.toString / bagDirName / "metadata" / "dataset.json") should exist

    // get dataset metadata
    authMocker.expectsUserFooBar
    get(metadataURI, headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe OK_200
      body shouldBe """{"titles":["blabla"],"privacySensitiveDataPresent":"unspecified","acceptDepositAgreement":false}"""
    }

    // invalidate the metadata and try again
    (testDir / "drafts" / "foo" / uuid / bagDirName / "metadata" / "dataset.json").write("---")
    authMocker.expectsUserFooBar
    get(metadataURI, headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe INTERNAL_SERVER_ERROR_500
    }
  }

  "scenario: POST /deposit twice; GET /deposit" should "return a list of datasets" in {
    // create two deposits
    val responseBodies: Seq[String] = (0 until 2).map { _ =>
      authMocker.expectsUserFooBar
      post(
        uri = s"/deposit",
        headers = Seq(fooBarBasicAuthHeader)
      ) {
        body
      }
    }
    responseBodies.foreach(_ should endWith("""Z"}"""))

    // list all deposits
    authMocker.expectsUserFooBar
    get(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe OK_200
      // random order
      responseBodies.foreach(body should include(_))
      body.length shouldBe responseBodies.mkString("[", ",", "]").length
      body should startWith("""[{""")
      body should endWith("""}]""")
    }
  }

  "scenario: POST /deposit; PUT /deposit/:uuid/file/...; GET /deposit/$uuid/file/..." should "return single FileInfo object respective an array of objects" in {
    authMocker.expectsUserFooBar
    val uuid = createDeposit

    // upload files in a folder (more variations in UploadSpec)
    val dataFilesBase = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid)).getDataFiles.get.bag.data

    // upload without content type
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/text.txt",
      headers = Seq(fooBarBasicAuthHeader),
      body = "Lorum ipsum"
    ) {
      body shouldBe "Content-Type is a mandatory request header and must not be application/zip nor start with multipart."
      status shouldBe BAD_REQUEST_400
    }

    // upload a file in a folder
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(fooBarBasicAuthHeader, contentTypePlainText),
      body = "Lorum ipsum"
    ) {
      status shouldBe CREATED_201
      (dataFilesBase / "path" / "to" / "text.txt").contentAsString shouldBe "Lorum ipsum"
    }

    // attempt to overwrite dir
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to",
      headers = Seq(fooBarBasicAuthHeader, contentTypePlainText),
      body = "Lorum ipsum"
    ) {
      status shouldBe CONFLICT_409
      body shouldBe "Attempt to overwrite a directory with a file."
    }

    // get file
    authMocker.expectsUserFooBar
    get(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(fooBarBasicAuthHeader, contentTypePlainText)
    ) {
      status shouldBe OK_200
      // a single json object: {"..."}, more details are tested in DataFilesSpec.fileInfo
      body should (fullyMatch regex """^\{.*"\}$""")
    }

    // get directory
    authMocker.expectsUserFooBar
    get(
      uri = s"/deposit/$uuid/file/path/",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      status shouldBe OK_200
      // a list of json objects: [{"..."}], more details are tested in DataFilesSpec.fileInfo
      body should (fullyMatch regex """^\[\{.*"\}\]$""")
    }
  }

  "scenario: POST /deposit; PUT /deposit/:uuid/state; PUT /deposit/$uuid/file/..." should "return forbidden cannot update SUBMITTED deposit" in {
    val uuid: String = setupSubmittedDeposit

    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to/test.txt", headers = Seq(fooBarBasicAuthHeader, ("Content-Type", "application/json")),
      body = "Lorum ipsum"
    ) {
      body shouldBe "Deposit has state SUBMITTED, can only update deposits with one of the states: DRAFT"
      status shouldBe FORBIDDEN_403
    }
  }

  "scenario: POST /deposit; twice GET /deposit/:uuid/doi" should "return 200" in {
    authMocker.expectsUserFooBar
    val uuid = createDeposit

    // expect a new doi once
    val doi = "12345"
    app.pidRequester.requestPid _ expects(*, *) once() returning Success(doi)
    val expectedDoiRecord = s"""{"doi":"$doi"}"""

    // get the doi twice
    authMocker.expectsUserFooBar
    get(
      uri = s"/deposit/$uuid/doi",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      status shouldBe OK_200
      body shouldBe expectedDoiRecord
    }
    authMocker.expectsUserFooBar
    get(uri = s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe OK_200
      body shouldBe expectedDoiRecord
    }
  }

  "scenario: create - ... - sumbit" should "create submitted dataset copied from a draft" in {
    val uuid: String = setupSubmittedDeposit

    // resubmit succeeds
    val props = testDir / "drafts" / "foo" / uuid / "deposit.properties"
    props.write(props.contentAsString.replace("SUBMITTED", "DRAFT"))
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/state",
      headers = Seq(fooBarBasicAuthHeader),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      status shouldBe NO_CONTENT_204
    }
    props.write(props.contentAsString.replace("DRAFT", "SUBMITTED"))

    // failing delete
    authMocker.expectsUserFooBar
    delete(uri = s"/deposit/$uuid", headers = Seq(fooBarBasicAuthHeader)) {
      body shouldBe "Deposit has state SUBMITTED, can only delete deposits with one of the states: DRAFT, ARCHIVED, REJECTED"
      status shouldBe FORBIDDEN_403
    }
    authMocker.expectsUserFooBar

    // deposit still exists
    get(
      uri = s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader),
    ) {
      body shouldBe """{"state":"SUBMITTED","stateDescription":"The deposit is being processed"}"""
      status shouldBe OK_200
    }
  }

  "scenario: create - sumbit" should "refuse a submit without DOI" in {
    (testDir / "stage").createDirectories()
    (testDir / "easy-ingest-flow-inbox").createDirectories()
    val metadataWithoutDOI = JsonUtil.toJson(
      DatasetMetadata(
        getManualTestResource("datasetmetadata-from-ui-all.json")).getOrRecover(e => fail(e)
      ).copy(identifiers = None)
    )

    // create dataset
    authMocker.expectsUserFooBar
    val uuid = createDeposit

    // upload dataset metadata
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/metadata",
      headers = Seq(fooBarBasicAuthHeader),
      body = metadataWithoutDOI
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204
    }

    // submit
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/state",
      headers = Seq(fooBarBasicAuthHeader),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      body shouldBe s"InvalidDoi: DOI must be obtained by calling GET /deposit/$uuid"
      status shouldBe BAD_REQUEST_400
    }

    // delete
    authMocker.expectsUserFooBar
    delete(uri = s"/deposit/$uuid", headers = Seq(fooBarBasicAuthHeader)) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204
    }

    // deposit no longer exist
    authMocker.expectsUserFooBar
    get(
      uri = s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader),
    ) {
      body shouldBe s"Deposit $uuid not found"
      status shouldBe NOT_FOUND_404
    }
  }

  "scenario: POST /deposit; hack state to ARCHIVED; SUBMIT" should "reject state transition" in {
    // create dataset
    authMocker.expectsUserFooBar
    val uuid = createDeposit

    // hack state
    val props = testDir / "drafts" / "foo" / uuid.toString / "deposit.properties"
    props.write(props.contentAsString.replace("DRAFT", "ARCHIVED"))

    // submit
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/state",
      headers = Seq(fooBarBasicAuthHeader),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      status shouldBe FORBIDDEN_403
      body shouldBe s"Cannot transition from ARCHIVED to SUBMITTED"
    }

    // submit did not complain about missing metadata, so the state transition check indeed came first
    (testDir / "drafts" / "foo" / uuid.toString / bagDirName / "metatada") shouldNot exist
  }

  private def setupSubmittedDeposit: String = {
    val datasetMetadata = getManualTestResource("datasetmetadata-from-ui-all.json")
    val doi = DatasetMetadata(datasetMetadata)
      .getOrRecover(fail(_))
      .doi
      .getOrElse(fail("could not get DOI from test input"))

    (testDir / "easy-ingest-flow-inbox").createDirectories()
    (testDir / "stage").createDirectories()

    // create dataset
    authMocker.expectsUserFooBar
    val uuid = createDeposit
    val depositDir = testDir / "drafts" / "foo" / uuid.toString

    // copy DOI from metadata into deposit.properties
    (depositDir / "deposit.properties").append(s"identifier.doi=$doi")

    // upload dataset metadata
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/metadata",
      headers = Seq(fooBarBasicAuthHeader),
      body = datasetMetadata
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204
    }

    // submit
    new PropertiesConfiguration((depositDir / "deposit.properties").toJava)
      .containsKey("bag-store.bag-id") shouldBe false
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204

      val updatesProps = new PropertiesConfiguration((depositDir / "deposit.properties").toJava)
      updatesProps.containsKey("bag-store.bag-id") shouldBe true

      // +4 is the difference in the number of files (and directories) in the metadata directory: json versus xml's
      (depositDir.walk().size + 4) shouldBe
        (testDir / "easy-ingest-flow-inbox" / updatesProps.getString("bag-store.bag-id")).walk().size
    }
    uuid
  }
}
