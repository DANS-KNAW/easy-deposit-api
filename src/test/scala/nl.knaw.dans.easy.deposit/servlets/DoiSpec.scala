/*
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

import better.files.File
import nl.knaw.dans.easy.deposit.PidRequester.PidType
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker
import nl.knaw.dans.easy.deposit.{ PidRequester, TestSupportFixture }
import org.eclipse.jetty.http.HttpStatus._

import scala.util.Success

class DoiSpec extends TestSupportFixture with ServletFixture {
  private def propsFile(uuid: String): File = testDir / "drafts/foo" / uuid / "deposit.properties"

  private def datasetMetadataFile(uuid: String): File = testDir / "drafts/foo" / uuid / bagDirName / "metadata" / "dataset.json"

  private val submit = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
  private val doi = "10.17632/DANS.6wg5xccnjd.1"
  private val doiForJson = s"""  "identifiers":[{"scheme":"id-type:DOI","value":"$doi"}]"""
  private val doiProperty = s"identifier.doi = $doi"
  private val mandatoryOnSubmit =
    """|  "titles": ["blabla"],
       |  "descriptions": ["rababera"],
       |  "creators": [ { "initials": "A", "surname": "Einstein", } ],
       |  "dates": [
       |    { "scheme": "dcterms:W3CDTF", "value": "2018-05-31", "qualifier": "dcterms:available" },
       |    { "scheme": "dcterms:W3CDTF", "value": "2018-05-31", "qualifier": "dcterms:created" },
       |  ],
       |  "license": { "scheme": "dcterms:URI", "value": "http://creativecommons.org/publicdomain/zero/1.0" },
       |  "audiences": [ { "scheme": "string", "value": "string", "key": "D33000" } ],
       |  "accessRights": "OPEN_ACCESS",
       |  "privacySensitiveDataPresent": "no",
       |  "acceptDepositAgreement": true,
       |""".stripMargin

  private def shouldReturnDOI = {
    body shouldBe s"""{"doi":"$doi"}"""
    status shouldBe OK_200
  }

  private def shouldReturnBadRequest(uuid: String) = {
    body shouldBe s"InvalidDoi: DOI must be obtained by calling GET /deposit/$uuid"
    status shouldBe BAD_REQUEST_400
  }

  private val mockedPidRequester: PidRequester = mock[PidRequester]
  mountDepositServlet(
    createTestApp(mockedPidRequester),
    AuthenticationMocker.expectsUserFooBarAnyNumberOfTimes,
  )

  "GET /deposit/{id}/doi" should "return a fresh DOI" in {
    val uuid = createDeposit
    mockedPidRequester.requestPid _ expects(*, PidType.doi) returning Success(doi) once()
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldReturnDOI }
  }

  it should "fail when json has a DOI and properties not" in {
    val uuid = createDeposit
    datasetMetadataFile(uuid).write(s"""{$doiForJson}""")
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when DOI's are different" in {
    val uuid = createDeposit
    datasetMetadataFile(uuid).write(s"""{$doiForJson}""")
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldReturnBadRequest(uuid) }
  }

  it should "return DOI when properties has a DOI but json not" in {
    // this can happen when a client asks twice for a DOI without saving the metadata in between
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty)
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldReturnDOI }
  }

  "PUT /deposit/{id}/metadata" should "succeed when DOI's are equal" in {
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty)
    put(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader), body = s"""{$doiForJson}""") { status shouldBe NO_CONTENT_204 }
  }

  it should "filter ezproxy from license URL (EASY-2878 ticket/10290)" in {
    val uuid = createDeposit
    val license = """{"license": { "scheme": "dcterms:URI", "value": "http://dans.knaw.nl.ezproxy2.something.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf"}}"""
    put(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader), body = license) { status shouldBe NO_CONTENT_204 }
    val datasetMetadata = datasetMetadataFile(uuid).contentAsString
    datasetMetadata should include ("http://dans.knaw.nl/en/about")
    datasetMetadata should include ("DANSLicence.pdf")
    datasetMetadata should not include "ezproxy"
    datasetMetadata should not include "something"
  }

  it should "succeed without any DOI" in {
    val uuid = createDeposit
    put(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader), body = "{}") { status shouldBe NO_CONTENT_204 }
  }

  it should "fail when DOI's are different" in {
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty + "xyz")
    put(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader), body = s"""{$doiForJson}""") { shouldReturnBadRequest(uuid) }
  }

  it should "fail when json has a DOI but properties not" in {
    val uuid = createDeposit
    put(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader), body = s"""{$doiForJson}""") { shouldReturnBadRequest(uuid) }
  }

  it should "fail when properties has a DOI but json not" in {
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty)
    put(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader), body = "{}") { shouldReturnBadRequest(uuid) }
  }

  "PUT /deposit/{id}/state" should "succeed when DOI's are equal" in {
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty)
    datasetMetadataFile(uuid).write(s"""{$doiForJson,$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { status shouldBe NO_CONTENT_204 }
  }

  it should "fail without any DOI" in {
    val uuid = createDeposit
    datasetMetadataFile(uuid).write(s"""{$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when DOI's are different" in {
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty + "xyz")
    datasetMetadataFile(uuid).write(s"""{$doiForJson,$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when json has a DOI but properties not" in {
    val uuid = createDeposit
    datasetMetadataFile(uuid).write(s"""{$doiForJson,$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when properties has a DOI but json not" in {
    val uuid = createDeposit
    datasetMetadataFile(uuid).write(s"""{$mandatoryOnSubmit}""")
    propsFile(uuid).append(doiProperty)
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }
}
