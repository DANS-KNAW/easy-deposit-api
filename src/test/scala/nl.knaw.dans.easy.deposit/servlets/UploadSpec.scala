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

import java.nio.file.Files.setPosixFilePermissions
import java.nio.file.attribute.PosixFilePermissions.fromString
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker.{ expectsUserFooBar, mockedAuthenticationProvider }
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.easy.deposit.{ DepositDir, EasyDepositApiApp, TestSupportFixture }
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class UploadSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    (testDir / "drafts").createDirectories()
    (testDir / "input").createDirectory()
  }

  private val basicAuthentication: (String, String) = ("Authorization", fooBarBasicAuthHeader)
  private val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
    override val pidRequester: PidRequester = null
  }
  mountServlets(app, mockedAuthenticationProvider)

  "POST" should "upload files from multiple form fields" in {
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
      ("some", "3.txt", "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"),
      ("more", "4.txt", "Ut enim ad minim veniam"),
    ))
    val uuid = createDataset
    val relativeTarget = "path/to/dir"
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(basicAuthentication),
      files = bodyParts
    ) {
      body shouldBe ""
      status shouldBe OK_200
      val uploaded = (testDir / "drafts/foo" / uuid.toString / "bag/data" / relativeTarget).list
      uploaded.size shouldBe bodyParts.size
      uploaded.foreach(file =>
        file.contentAsString shouldBe (testDir / "input" / file.name).contentAsString
      )
    }
  }

  it should "report upload failure" in {
    // need at least two files to show lazy evaluation with the logging: the second file is not tried to process
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
    ))
    val uuid = createDataset
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()

    def setTargetPermissions(value: String) = setPosixFilePermissions(absoluteTarget.path, fromString(value))

    setTargetPermissions("r--r--r--") // cause an upload failure
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(basicAuthentication),
      files = bodyParts
    ) {
      setTargetPermissions("rwxr-xr-x") // restore to allow clean up
      body shouldBe "Internal Server Error"
      status shouldBe INTERNAL_SERVER_ERROR_500
    }
  }

  it should "report ZIP item found after uploading another" in {
    // need at least two files to show lazy evaluation with the logging: the second file is not tried to process
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.zip", "content doesn't matter"),
    ))
    val uuid = createDataset
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(basicAuthentication),
      files = bodyParts
    ) {
      body shouldBe "A multipart/form-data message contained a ZIP [2.zip] part but also other parts. At least one part has been uploaded."
      status shouldBe CONFLICT_409
      absoluteTarget.list.size shouldBe 1
    }
  }

  it should "report ZIP item found without uploading any of the others" in {
    val bodyParts = createBodyParts(Seq(
      ("some", "1.zip", "content doesn't matter"),
      ("some", "2.txt", "Lorem ipsum dolor sit amet"),
    ))
    val uuid = createDataset
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(basicAuthentication),
      files = bodyParts
    ) {
      body shouldBe "A multipart/form-data message contained a ZIP [1.zip] part but also other parts. Nothing has been uploaded."
      status shouldBe CONFLICT_409
      absoluteTarget.list.size shouldBe 0
    }
  }

  it should "report a malformed ZIP" in {
    val bodyParts = createBodyParts(Seq(("some", "1.zip", "invalid zip content")))
    val uuid = createDataset
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(basicAuthentication),
      files = bodyParts
    ) {
      absoluteTarget.list.size shouldBe 0
      status shouldBe BAD_REQUEST_400
      body shouldBe s"ZIP file is malformed. No entries found."
    }
  }

  it should "extract all files from a ZIP" in {
    File("src/test/resources/manual-test/Archive.zip").copyTo(testDir / "input" / "1.zip")
    val uuid = createDataset
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()
    absoluteTarget.list.size shouldBe 0 // precondition
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(basicAuthentication),
      files = Seq(("formFieldName", (testDir / "input/1.zip").toJava))
    ) {
      body shouldBe ""
      status shouldBe OK_200
      absoluteTarget.walk().map(_.path.getFileName.toString).toList should contain theSameElementsAs
        List("dir", "login.html", "readme.md", "__MACOSX", "._login.html", "upload.html")
    }
  }

  it should "report a missing content disposition" in {
    val uuid = createDataset

    // upload a file
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/path/to/",
      headers = Seq(basicAuthentication),
      body = "Lorem ipsum dolor sit amet"
    ) {
      status shouldBe BAD_REQUEST_400
      body shouldBe """Must have a Content-Type starting with "multipart/", got None."""
    }
  }

  it should "report an invalid content disposition" in {
    val uuid = createDataset

    // upload a file
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/path/to/",
      headers = Seq(basicAuthentication, ("Content-Type", "text/plain")),
      body = "Lorem ipsum dolor sit amet"
    ) {
      status shouldBe BAD_REQUEST_400
      body shouldBe """Must have a Content-Type starting with "multipart/", got Some(text/plain)."""
    }
  }

  s"PUT" should "return 201 for a new respectively 200 for a replaced file" in {
    val uuid = createDataset

    val bagBase = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid.toString)).getDataFiles.get.bag
    val shortContent = "Lorum ipsum"
    val longContent = "dolor sit amet"

    // first upload
    expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(basicAuthentication),
      body = longContent
    ) {
      status shouldBe CREATED_201
      (bagBase / "data/path/to/text.txt").contentAsString shouldBe longContent
      (bagBase / "manifest-sha1.txt").contentAsString shouldBe "0d21d1af59b36f0bee70fd034e931ec72f04f1cd  data/path/to/text.txt\n"
    }

    // second upload of same file
    expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(basicAuthentication),
      body = shortContent
    ) {
      status shouldBe OK_200
      (bagBase / "data/path/to/text.txt").contentAsString shouldBe shortContent
      (bagBase / "manifest-sha1.txt").contentAsString shouldBe "c5b8de8cc3587aef4e118a481115391033621e06  data/path/to/text.txt\n"
    }
  }

  /**
   * Mimics field values for: `<form method="post" enctype="multipart/form-data">`
   *
   * @param files tuples of:
   *              - name in `<input type="file" name="some">`
   *              - simple file name
   *              - content of the file to create
   * @return
   */
  private def createBodyParts(files: Seq[(String, String, String)]): Seq[(String, java.io.File)] = {
    files.foreach { case (_, file, content) => (testDir / "input" / file).write(content) }
    files.map { case (formField, file, _) => (formField, (testDir / "input" / file).toJava) }
  }

  private def createDataset: UUID = {
    expectsUserFooBar
    val responseBody = post(
      uri = s"/deposit",
      headers = Seq(basicAuthentication)
    ) {
      new String(bodyBytes)
    }
    DepositInfo(responseBody)
      .map(_.id)
      .getOrRecover(e => fail(s"preparing a dataset failed: $e", e))
  }
}
