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
import nl.knaw.dans.easy.deposit.servlets
import org.eclipse.jetty.http.HttpStatus._

class NotFoundSpec extends DepositServletFixture {

  private val state =
    """{
      |  "state": "DRAFT",
      |  "stateDescription": "Deposit is open for modification."
      |}""".stripMargin
  private val fileBodyParts = {
    bodyParts(testDir / "input", Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
    ))
  }

  private def shouldBeNoSuchDeposit = {
    status shouldBe NOT_FOUND_404
    body shouldBe s"Deposit $uuid not found"
  }

  private def shouldBeNotFound(msg: String) = {
    status shouldBe NOT_FOUND_404
    body shouldBe msg
  }

  private def shouldBeInternalServerError = {
    // TODO replace these three with: should matchPattern
    status shouldBe INTERNAL_SERVER_ERROR_500
    body shouldBe "Internal Server Error"
  }

  "404 deposit not found" should "be returned by GET /deposit/{id}/metadata" in {
    get(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by PUT /deposit/{id}/metadata" in {
    put(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader), body = "{}") { shouldBeNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/doi" in {
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/state" in {
    get(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by PUT /deposit/{id}/state" in {
    put(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader), body = state) { shouldBeNoSuchDeposit }
  }
  it should "be returned by DELETE /deposit/{id}" in {
    delete(s"/deposit/$uuid", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/file/{dir_path}" in {
    get(s"/deposit/$uuid/file/path/to/dir", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by POST /deposit/{id}/file/{dir_path}" in {
    post(s"/deposit/$uuid/file/path/to/dir", params = Iterable(), headers = Seq(fooBarBasicAuthHeader), files = fileBodyParts) { shouldBeNoSuchDeposit }
  }
  it should "be returned by DELETE /deposit/{id}/file/{dir_path}" in {
    delete(s"/deposit/$uuid/file/path/to/dir", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by GET /deposit/{id}/file/{file_path}" in {
    get(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by PUT /deposit/{id}/file/{file_path}" in {
    put(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(fooBarBasicAuthHeader, contentTypePlainText)) { shouldBeNoSuchDeposit }
  }
  it should "be returned by DELETE /deposit/{id}/file/{file_path}" in {
    delete(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNoSuchDeposit }
  }

  "404 other not found" should "be returned by GET /deposit/{id}/file/{dir_path}" in {
    val uuid = createDeposit
    get(s"/deposit/$uuid/file/path/to/dir", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNotFound("path/to/dir") }
  }
  it should "be returned by DELETE /deposit/{id}/file/{dir_path}" in {
    val uuid = createDeposit
    delete(s"/deposit/$uuid/file/path/to/dir", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNotFound("path/to/dir") }
  }
  it should "be returned by GET /deposit/{id}/file/{file_path}" in {
    val uuid = createDeposit
    get(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNotFound("path/to/file.txt") }
  }
  it should "be returned by DELETE /deposit/{id}/file/{file_path}" in {
    val uuid = createDeposit
    delete(s"/deposit/$uuid/file/path/to/file.txt", headers = Seq(fooBarBasicAuthHeader)) { shouldBeNotFound("path/to/file.txt") }
  }
  // TODO files/directories on the file system but not in files.xml and/or the other way around

  "410 Gone" should "be returned by GET /deposit/{id}/metadata" ignore { // TODO clean up not implemented?
    get(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader)) { status shouldBe GONE_410 }
  }

  private def propsFile(uuid: String): File = testDir / "drafts/foo" / uuid / "deposit.properties"

  private def jsonFile(uuid: String): File = testDir / "drafts/foo" / uuid / "bag/metadata/dataset.json"

  "500 on missing metadata" should "be returned by GET /deposit/{id}/state (no file)" in {
    val uuid = createDeposit
    propsFile(uuid).delete()
    get(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader)) { shouldBeInternalServerError }
  }

  it should "be returned by GET /deposit/{id}/state (no property)" in {
    val uuid = createDeposit
    propsFile(uuid).write("foo = bar")
    get(s"/deposit/$uuid/state", headers = Seq(fooBarBasicAuthHeader)) { shouldBeInternalServerError }
  }

  it should "be returned by GET /deposit/{id}/metadata" in {
    val uuid = createDeposit
    jsonFile(uuid).delete()
    get(s"/deposit/$uuid/metadata", headers = Seq(fooBarBasicAuthHeader)) { shouldBeInternalServerError }
  }

  it should "be returned by GET /deposit/{id}/doi (no json)" in {
    val uuid = createDeposit
    jsonFile(uuid).delete()
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldBeInternalServerError }
  }

  it should "be returned by GET /deposit/{id}/doi (no properties)" in {
    val uuid = createDeposit
    jsonFile(uuid).write("""{"identifiers":[{"scheme":"id-type:DOI","value":"10.17632/DANS.6wg5xccnjd.1"}]}""")
    propsFile(uuid).delete()
    get(s"/deposit/$uuid/doi", headers = Seq(fooBarBasicAuthHeader)) { shouldBeInternalServerError }
  }
}
