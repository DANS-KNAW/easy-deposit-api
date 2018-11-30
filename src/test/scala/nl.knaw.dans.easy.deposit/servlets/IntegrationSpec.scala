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

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, JsonUtil }
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Success

class IntegrationSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  private val authMocker = new AuthenticationMocker(){}
  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
    override val pidRequester: PidRequester = mock[PidRequester]
  }
  mountServlets(app, authMocker.mockedAuthenticationProvider)

  s"scenario: /deposit/:uuid/metadata life cycle" should "return default dataset metadata" in {

    // create dataset
    authMocker.expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) { body }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))
    val metadataURI = s"/deposit/$uuid/metadata"

    // create dataset metadata
    assumeSchemaAvailable
    authMocker.expectsUserFooBar
    put(
      metadataURI, headers = Seq(fooBarBasicAuthHeader),
      body = """{"titles":["blabla"]}""" // more variations in DepositDirSpec
    ) {
      status shouldBe NO_CONTENT_204
    }
    (testDir / "drafts" / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json") should exist

    // get dataset metadata
    authMocker.expectsUserFooBar
    get(metadataURI, headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe OK_200
      body shouldBe """{"titles":["blabla"],"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
    }

    // invalidate the metadata and try again
    (testDir / "drafts" / "foo" / uuid / "bag" / "metadata" / "dataset.json").write("---")
    authMocker.expectsUserFooBar
    get(metadataURI, headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe INTERNAL_SERVER_ERROR_500
    }
  }

  s"POST /deposit/:uuid/metadata" should "return a user friendly message" in {
    // more variants in DDMSpec and DatasetMetadataSpec, here we test the full chain of error handling

    // create dataset
    authMocker.expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) { body }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))
    val metadataURI = s"/deposit/$uuid/metadata"

    // create dataset metadata
    assumeSchemaAvailable
    authMocker.expectsUserFooBar
    put(
      metadataURI, headers = Seq(fooBarBasicAuthHeader),
      body = """{"spatialPoints": [{ "scheme": "RD", "x": "795,00", "y": "446750Z" }]}"""
    ) {
      body shouldBe """Bad Request. invalid DatasetMetadata: requirement failed: Invalid number [795,00]; got {"scheme":"RD","x":"795,00","y":"446750Z"} SpatialPoint"""
      status shouldBe BAD_REQUEST_400
    }
  }

  s"scenario: POST /deposit twice; GET /deposit" should "return a list of datasets" in {

    // create two deposits
    val responseBodies: Seq[String] = (0 until 2).map { _ =>
      authMocker.expectsUserFooBar
      post(
        uri = s"/deposit",
        headers = Seq(fooBarBasicAuthHeader)
      ) { new String(bodyBytes) }
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

    // create dataset
    authMocker.expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) { body }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

    val dataFilesBase = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid)).getDataFiles.get.bag.data

    // upload a file in a folder
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(fooBarBasicAuthHeader),
      body = "Lorum ipsum"
    ) {
      status shouldBe CREATED_201
      (dataFilesBase / "path" / "to" / "text.txt").contentAsString shouldBe "Lorum ipsum"
    }

    val expectedItem = """{"filename":"text.txt","dirpath":"path/to/text.txt","sha1sum":"c5b8de8cc3587aef4e118a481115391033621e06"}"""
    val expectedListItem = expectedItem.replace("/text.txt", "")

    // get file
    authMocker.expectsUserFooBar
    get(uri = s"/deposit/$uuid/file/path/to/text.txt", headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe OK_200
      body shouldBe expectedItem
    }

    // get directory
    authMocker.expectsUserFooBar
    get(uri = s"/deposit/$uuid/file/path/", headers = Seq(fooBarBasicAuthHeader)) {
      status shouldBe OK_200
      body shouldBe s"""[$expectedListItem]"""
    }
  }

  s"scenario: POST /deposit; twice GET /deposit/:uuid/doi" should "return 200" in {

    // create dataset
    authMocker.expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) { body }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

    // expect a new doi once
    val doi = "12345"
    (app.pidRequester.requestPid(_: PidType)) expects * once() returning Success(doi)
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

  s"scenario: create - ... - sumbit" should "create submitted dataset copied from a draft" in {

    val datasetMetadata = getManualTestResource("datasetmetadata-from-ui-all.json")
    val doi = DatasetMetadata(datasetMetadata)
      .getOrRecover(fail(_))
      .doi
      .getOrElse(fail("could not get DOI from test input"))

    (testDir / "easy-ingest-flow-inbox").createDirectories()
    (testDir / "stage").createDirectories()

    // create dataset
    authMocker.expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) { body }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))
    val depositDir = testDir / "drafts" / "foo" / uuid.toString

    // copy DOI from metadata into deposit.properties
    (depositDir / "deposit.properties").append(s"identifier.doi=$doi")

    // upload dataset metadata
    assumeSchemaAvailable
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
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204

      // +3 is difference in number of files in metadata directory: json versus xml's
      (depositDir.walk().size + 3) shouldBe (testDir / "easy-ingest-flow-inbox" / uuid.toString).walk().size
    }
  }

  s"scenario: create - sumbit" should "refuse a submit without DOI" in {
    (testDir / "stage").createDirectories()
    (testDir / "easy-ingest-flow-inbox").createDirectories()
    val metadataWithoutDOI = JsonUtil.toJson(
      DatasetMetadata(
        getManualTestResource("datasetmetadata-from-ui-all.json")).getOrRecover(e => fail(e)
      ).copy(identifiers = None)
    )

    // create dataset
    authMocker.expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) { body }

    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))
    // upload dataset metadata
    assumeSchemaAvailable
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
  }

  s"scenario: POST /deposit; hack state to ARCHIVED; SUBMIT" should "reject state transition" in {

    // create dataset
    authMocker.expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(fooBarBasicAuthHeader)) { body }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

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
      body shouldBe s"Cannot transition from ARCHIVED to SUBMITTED (deposit id: $uuid, user: foo)"
    }

    // submit did not complain about missing metadata, so the state transition check indeed came first
    (testDir / "drafts" / "foo" / uuid.toString / "bag" / "metatada") shouldNot exist
  }
}
