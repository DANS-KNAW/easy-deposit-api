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
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo }
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.{ Success, Try }

class IntegrationSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
    override val pidRequester: PidRequester = mock[PidRequester]
  }
  mountServlets(app, mockedAuthenticationProvider)

  private val basicAuthentication: (String, String) = ("Authorization", fooBarBasicAuthHeader)

  s"scenario: /deposit/:uuid/metadata life cycle" should "return default dataset metadata" in {

    // create dataset
    expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      new String(bodyBytes)
    }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))
    val metadataURI = s"/deposit/$uuid/metadata"

    // create dataset metadata
    expectsUserFooBar
    put(metadataURI, headers = Seq(basicAuthentication),
      body = """{"titles":["blabla"]}""" // more variations in DepositDirSpec
    ) {
      status shouldBe NO_CONTENT_204
    }
    (testDir / "drafts" / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json").toJava should exist

    // get dataset metadata
    expectsUserFooBar
    get(metadataURI, headers = Seq(basicAuthentication)) {
      status shouldBe OK_200
      body shouldBe """{"titles":["blabla"],"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
    }

    // invalidate the metadata and try again
    val mdFile = {
      val dd = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid))
      (dd.baseDir / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json").write("---")
    }
    expectsUserFooBar
    get(metadataURI, headers = Seq(basicAuthentication)) {
      status shouldBe INTERNAL_SERVER_ERROR_500
    }

    // remove the metadata and try another time
    mdFile.delete() // TODO replace with "DELETE deposit/:uuid" when implemented
    expectsUserFooBar
    get(metadataURI, headers = Seq(basicAuthentication)) {
      status shouldBe NOT_FOUND_404
    }
  }


  s"scenario: POST /deposit twice; GET /deposit" should "return a list of datasets" in {

    // create two deposits
    val responseBodies: Seq[String] = (0 until 2).map { _ =>
      expectsUserFooBar
      post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
        new String(bodyBytes)
      }
    }
    responseBodies.foreach(_ should endWith("""Z"}"""))

    // list all deposits
    expectsUserFooBar
    get(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      status shouldBe OK_200
      // random order
      responseBodies.foreach(body should include(_))
      body.length shouldBe responseBodies.mkString("[", ",", "]").length
      body should startWith("""[{""")
      body should endWith("""}]""")
    }
  }

  s"scenario: POST /deposit; twice POST /deposit/:uuid/file/path/to/text.txt" should "return 201 respectively 200" in {

    // create dataset
    expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      new String(bodyBytes)
    }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

    val dataFilesBase = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid)).getDataFiles.get.bag.data
    val times = 500
    val expectedContentSize = 37 * times - 1

    // upload the file twice
    expectsUserFooBar
    put(uri = s"/deposit/$uuid/file/path/to/text.txt", headers = Seq(basicAuthentication), body = randomContent(times)) {
      status shouldBe CREATED_201
      (dataFilesBase / "path" / "to" / "text.txt").size shouldBe expectedContentSize
    }
    expectsUserFooBar
    put(uri = s"/deposit/$uuid/file/path/to/text.txt", headers = Seq(basicAuthentication), body = "Lorum ipsum") {
      status shouldBe OK_200
      (dataFilesBase / "path" / "to" / "text.txt").size shouldBe 11
    }
    expectsUserFooBar
    get(uri = s"/deposit/$uuid/file/path", headers = Seq(basicAuthentication)) {
      status shouldBe OK_200
      body shouldBe """[{"fileName":"text.txt","dirPath":"path/to","sha1sum":"c5b8de8cc3587aef4e118a481115391033621e06"}]"""
    }
  }

  s"scenario: POST /deposit; twice GET /deposit/:uuid/doi" should "return 200" in {

    // create dataset
    expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      new String(bodyBytes)
    }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

    // expect a new doi once
    val doi = "12345"
    (app.pidRequester.requestPid(_: PidType)) expects * once() returning Success(doi)
    val expectedDoiRecord = s"""{"doi":"$doi"}"""

    // get the doi twice
    expectsUserFooBar
    get(uri = s"/deposit/$uuid/doi", headers = Seq(basicAuthentication)) {
      status shouldBe OK_200
      body shouldBe expectedDoiRecord
    }
    expectsUserFooBar
    get(uri = s"/deposit/$uuid/doi", headers = Seq(basicAuthentication)) {
      status shouldBe OK_200
      body shouldBe expectedDoiRecord
    }
  }

  s"scenario: create - ... - sumbit" should "create submitted dataset copied from a draft" in {

    val datasetMetadata = getManualTestResource("datasetmetadata-from-ui-all.json")
    val doi = Try { DatasetMetadata(datasetMetadata).get.identifiers.get.headOption.get.value }
      .getOrRecover(e => fail("could not get DOI from test input", e))
    (testDir / "easy-ingest-flow-inbox").createDirectories()

    // create dataset
    expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      new String(bodyBytes)
    }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

    // upload dataset metadata
    expectsUserFooBar
    put(s"/deposit/$uuid/metadata", headers = Seq(basicAuthentication),
      body = datasetMetadata
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204
    }

    // upload a file
    expectsUserFooBar
    val headers = Seq(
      ("Content-Disposition", "text.txt"),
      ("Content-Type", "application/octet-stream"),
      basicAuthentication
    )
    post(uri = s"/deposit/$uuid/file/path/to/", headers = headers, body = randomContent(22)) {
      status shouldBe CREATED_201
    }
    // path is assembled from uri + header:
    (testDir / "drafts" / "foo" / uuid.toString / "bag/data/path/to/text.txt").toJava should exist

    // avoid having to mock the pid-service
    (testDir / "drafts" / "foo" / uuid.toString / "deposit.properties").append(s"identifier.doi=$doi")

    // submit
    expectsUserFooBar
    put(s"/deposit/$uuid/state", headers = Seq(basicAuthentication),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204
    }

    // +3 is difference in number of files in metadata directory: json versus xml's
    ((testDir / "drafts" / "foo" / uuid.toString).walk().size + 3) shouldBe (testDir / "easy-ingest-flow-inbox" / uuid.toString).walk().size
  }


  s"scenario: create - POST payload" should "report a missing content disposistion" in {

    val datasetMetadata = getManualTestResource("datasetmetadata-from-ui-all.json")
    val doi = Try { DatasetMetadata(datasetMetadata).get.identifiers.get.headOption.get.value }
      .getOrRecover(e => fail("could not get DOI from test input", e))
    (testDir / "easy-ingest-flow-inbox").createDirectories()

    // create dataset
    expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      new String(bodyBytes)
    }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

    // upload a file
    expectsUserFooBar
    post(uri = s"/deposit/$uuid/file/path/to/", headers = Seq(basicAuthentication), body = randomContent(22)) {
      status shouldBe BAD_REQUEST_400
      body shouldBe "Expecting header 'Content-Type: application/zip' or 'Content-Type: application/octet-stream'; the latter with a 'Content-Disposition'. GOT: None AND None"
    }
  }

  s"scenario: POST /deposit; hack state to ARCHIVED; SUBMIT" should "reject state transition" in {

    // create dataset
    expectsUserFooBar
    val responseBody = post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      new String(bodyBytes)
    }
    val uuid = DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))

    // hack state
    val props = testDir / "drafts" / "foo" / uuid.toString / "deposit.properties"
    props.write(props.contentAsString.replace("DRAFT", "ARCHIVED"))

    // submit
    expectsUserFooBar
    put(s"/deposit/$uuid/state", headers = Seq(basicAuthentication),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      status shouldBe FORBIDDEN_403
      body shouldBe s"Cannot transition from ARCHIVED to SUBMITTED (deposit id: $uuid, user: foo)"
    }

    // submit did not complain about missing metadata, so the state transition check indeed came first
    (testDir / "drafts" / "foo" / uuid.toString / "bag" / "metatada").toJava shouldNot exist
  }

  private def randomContent(times: Int) = {
    (0 until times).map(_ => UUID.randomUUID().toString).mkString("\n")
  }
}
