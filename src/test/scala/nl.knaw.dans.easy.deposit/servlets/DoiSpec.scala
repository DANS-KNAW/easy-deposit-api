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

import better.files.File
import nl.knaw.dans.easy.deposit.docs._
import org.eclipse.jetty.http.HttpStatus._

class DoiSpec extends DepositServletFixture {
  private def propsFile(uuid: String): File = testDir / "drafts/foo" / uuid / "deposit.properties"

  private def jsonFile(uuid: String): File = testDir / "drafts/foo" / uuid / "bag/metadata/dataset.json"

  private val submit = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
  private val doi = "10.17632/DANS.6wg5xccnjd.1"
  private val doiForJson =s"""  "identifiers":[{"scheme":"id-type:DOI","value":"$doi"}]"""
  private val doiProperty = s"identifier.doi = $doi"
  private val mandatoryOnSubmit =
    """|  "titles": ["blabla"],
       |  "descriptions": ["rababera"],
       |  "creators": [ { "initials": "A", "surname": "Einstein", } ],
       |  "dates": [
       |    { "scheme": "dcterms:W3CDTF", "value": "2018-05-31", "qualifier": "dcterms:available" },
       |    { "scheme": "dcterms:W3CDTF", "value": "2018-05-31", "qualifier": "dcterms:created" },
       |  ],
       |  "audiences": [ { "scheme": "string", "value": "string", "key": "D33000" } ],
       |  "accessRights": {"category": "OPEN_ACCESS"},
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

  "GET /deposit/{id}/doi" should "return a fresh DOI" in {
    val uuid = createDeposit
    mockDoiRequest(doi) once()
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldReturnDOI }
  }

  it should "fail when json has a DOI and properties not" in {
    val uuid = createDeposit
    jsonFile(uuid).write(s"""{$doiForJson}""")
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when DOI's are different" in {
    val uuid = createDeposit
    jsonFile(uuid).write(s"""{$doiForJson}""")
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
    assume(DDM.triedSchema.isAvailable)
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty)
    jsonFile(uuid).write(s"""{$doiForJson,$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { status shouldBe NO_CONTENT_204 }
  }

  it should "fail without any DOI" in {
    val uuid = createDeposit
    jsonFile(uuid).write(s"""{$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when DOI's are different" in {
    val uuid = createDeposit
    propsFile(uuid).append(doiProperty + "xyz")
    jsonFile(uuid).write(s"""{$doiForJson,$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when json has a DOI but properties not" in {
    val uuid = createDeposit
    jsonFile(uuid).write(s"""{$doiForJson,$mandatoryOnSubmit}""")
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }

  it should "fail when properties has a DOI but json not" in {
    val uuid = createDeposit
    jsonFile(uuid).write(s"""{$mandatoryOnSubmit}""")
    propsFile(uuid).append(doiProperty)
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = submit) { shouldReturnBadRequest(uuid) }
  }
}
