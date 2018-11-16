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
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.authentication.AuthenticationProvider
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, TestSupportFixture }
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class NotFoundSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  private val depositServlet = {
    val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
      override val pidRequester: PidRequester = mock[PidRequester]
    }
    new DepositServlet(app) {
      override def getAuthenticationProvider: AuthenticationProvider = {
        mockedAuthenticationProvider
      }
    }
  }
  addServlet(depositServlet, "/deposit/*")

  override def beforeEach(): Unit = {
    super.beforeEach()
    expectsUserFooBar
  }

  private val auth: (String, String) = ("Authorization", fooBarBasicAuthHeader)
  private val state =
    """{
      |  "state": "DRAFT",
      |  "stateDescription": "Deposit is open for modification."
      |}""".stripMargin
  private val bodyParts = {
    (testDir / "input").createDirectories()
    Seq(//TODO move method from  UploadSpec to fixture (TestSupportFixture not related to servlets, ServletsFixture has no testDir)
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
    ).map { case (formField, file, content) =>
      (testDir / "input" / file).write(content)
      (formField, (testDir / "input" / file).toJava)
    }
  }

  private def expectingNoSuchDeposit = {
    status shouldBe NOT_FOUND_404
    body shouldBe s"Deposit $uuid not found"
  }

  private def expectingNotFound(msg: String) = {
    status shouldBe NOT_FOUND_404
    body shouldBe msg
  }

  private def createDeposit = {
    val responseBody = post(s"/deposit/", headers = Seq(auth)) { body }
    expectsUserFooBar // another time for the actual test
    DepositInfo(responseBody).map(_.id.toString).getOrRecover(e => fail(e.toString, e))
  }

  "404 deposit not found" should "be returned by GET /deposit/{id}/metadata" in {
    get(s"/deposit/$uuid/metadata", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by PUT /deposit/{id}/metadata" in {
    put(s"/deposit/$uuid/metadata", headers = Seq(auth), body = "{}") { expectingNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/doi" in {
    get(s"/deposit/$uuid/doi", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/state" in {
    get(s"/deposit/$uuid/state", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by PUT /deposit/{id}/state" in {
    put(s"/deposit/$uuid/state", headers = Seq(auth), body = state) { expectingNoSuchDeposit }
  }
  it should "be returned by DELETE /deposit/{id}" in {
    delete(s"/deposit/$uuid", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/file/{dir_path}" in {
    get(s"/deposit/$uuid/file/path/to/dir", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by POST /deposit/{id}/file/{dir_path}" in {
    post(s"/deposit/$uuid/file/path/to/dir", params = Iterable(), headers = Seq(auth), files = bodyParts) { expectingNoSuchDeposit }
  }
  it should "be returned by DELETE /deposit/{id}/file/{dir_path}" in {
    delete(s"/deposit/$uuid/file/path/to/dir", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/file/{file_path}" in {
    get(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by PUT /deposit/{id}/file/{file_path}" in {
    put(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(auth)) { expectingNoSuchDeposit }
  }
  it should "be returned by DELETE /deposit/{id}/file/{file_path}" in {
    delete(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(auth)) { expectingNoSuchDeposit }
  }

  "404 other not found" should "be returned by GET /deposit/{id}/file/{dir_path}" in {
    val uuid = createDeposit
    get(s"/deposit/$uuid/file/path/to/dir", headers = Seq(auth)) { expectingNotFound("path/to/dir not found") }
  }
  it should "be returned by DELETE /deposit/{id}/file/{dir_path}" in {
    val uuid = createDeposit
    delete(s"/deposit/$uuid/file/path/to/dir", headers = Seq(auth)) { expectingNotFound("path/to/dir not found") }
  }
  it should "be returned by GET /deposit/{id}/file/{file_path}" in {
    val uuid = createDeposit
    get(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(auth)) { expectingNotFound("path/to/file.txt not found") }
  }
  it should "be returned by DELETE /deposit/{id}/file/{file_path}" in {
    val uuid = createDeposit
    delete(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(auth)) { expectingNotFound("path/to/file.txt not found") }
  }
  // TODO files/directories on the file system but not in files.xml and/or the other way around

  "410 Gone" should "be returned by GET /deposit/{id}/metadata" ignore { // TODO clean up not implemented?
    get(s"/deposit/$uuid/metadata", headers = Seq(auth)) { status shouldBe GONE_410 }
  }

  private val doi = "10.17632/DANS.6wg5xccnjd.1"
  private val jsonWithDoi = """{"identifiers":[{"scheme":"id-type:DOI","value":"doi:""" + doi + """"}]}"""

  private def propsFile(uuid: String): File = testDir / "drafts/foo" / uuid / "deposit.properties"

  private def jsonFile(uuid: String): File = testDir / "drafts/foo" / uuid / "bag/metadata/dataset.json"

  "500 on missing metadata" should "be returned by GET /deposit/{id}/state (no file)" in {
    val uuid = createDeposit
    propsFile(uuid).delete()
    get(s"/deposit/$uuid/state", headers = Seq(auth)) { status shouldBe INTERNAL_SERVER_ERROR_500 }
  }

  it should "be returned by GET /deposit/{id}/state (no property)" in {
    val uuid = createDeposit
    propsFile(uuid).write("")
    get(s"/deposit/$uuid/state", headers = Seq(auth)) { status shouldBe INTERNAL_SERVER_ERROR_500 }
  }

  it should "be returned by GET /deposit/{id}/metadata" in {
    val uuid = createDeposit
    jsonFile(uuid).delete()
    get(s"/deposit/$uuid/metadata", headers = Seq(auth)) { status shouldBe INTERNAL_SERVER_ERROR_500 }
  }

  it should "be returned by GET /deposit/{id}/doi (no json)" in {
    val uuid = createDeposit
    jsonFile(uuid).delete()
    get(s"/deposit/$uuid/doi", headers = Seq(auth)) { status shouldBe INTERNAL_SERVER_ERROR_500 }
  }

  it should "be returned by GET /deposit/{id}/doi (no properties)" in {
    val uuid = createDeposit
    jsonFile(uuid).write(jsonWithDoi)
    propsFile(uuid).delete()
    get(s"/deposit/$uuid/doi", headers = Seq(auth)) { status shouldBe INTERNAL_SERVER_ERROR_500 }
  }

  it should "be returned by GET /deposit/{id}/doi (not in json)" in {
    val uuid = createDeposit
    propsFile(uuid).append(s"identifier.doi = $doi")
    get(s"/deposit/$uuid/doi", headers = Seq(auth)) { status shouldBe INTERNAL_SERVER_ERROR_500 }
  }

  it should "be returned by GET /deposit/{id}/doi (not in properties)" in {
    val uuid = createDeposit
    jsonFile(uuid).write(jsonWithDoi)
    get(s"/deposit/$uuid/doi", headers = Seq(auth)) { status shouldBe INTERNAL_SERVER_ERROR_500 }
  }
}
