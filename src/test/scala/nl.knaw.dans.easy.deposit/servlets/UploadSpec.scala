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

import java.nio.file.attribute.PosixFilePermission
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.DepositDir
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._

class UploadSpec extends DepositServletFixture {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    (testDir / "drafts").createDirectories()
    (testDir / "input").createDirectory()
  }

  "POST" should "upload files from multiple form fields" in {
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
      ("some", "3.txt", "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"),
      ("more", "4.txt", "Ut enim ad minim veniam"),
    ))
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      body shouldBe ""
      status shouldBe OK_200
      val bagDir = testDir / "drafts/foo" / uuid.toString / "bag"
      val uploaded = (bagDir / "data" / relativeTarget).list
      uploaded.size shouldBe bodyParts.size
      uploaded.foreach(file =>
        file.contentAsString shouldBe (testDir / "input" / file.name).contentAsString
      )
      (bagDir / "manifest-sha1.txt").lines.size shouldBe bodyParts.size
      (testDir / "stage-upload").list.size shouldBe 0
    }
  }

  it should "refuse concurrent posts" in {
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
      ("some", "3.txt", "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"),
      ("more", "4.txt", "Ut enim ad minim veniam"),
    ))
    val uuid = createDeposit
    (testDir / s"stage-upload/foo-$uuid-XYZ").createDirectories() // mocks a concurrent post
    post(
      uri = s"/deposit/$uuid/file/path/to/dir",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      status shouldBe CONFLICT_409
      body shouldBe "Another upload is pending. Please try again later."
      val bagDir = testDir / "drafts/foo" / uuid.toString / "bag"
      (bagDir / "data").list.size shouldBe 0
      (bagDir / "manifest-sha1.txt").lines.size shouldBe 0
      (testDir / "stage-upload").list.size shouldBe 1
    }
  }

  it should "report upload failure" in {
    // need at least two files to show lazy evaluation with the logging: the second file is not tried to process
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
    ))
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget
    absoluteTarget
      .createDirectories()
      .removePermission(PosixFilePermission.OWNER_WRITE)

    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
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
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      body shouldBe "A multipart/form-data message contained a ZIP [2.zip] part but also other parts."
      status shouldBe BAD_REQUEST_400
      absoluteTarget.list.size shouldBe 0 // preceding plain file not added to draft bag
    }
  }

  it should "report ZIP item found without uploading any of the others" in {
    val bodyParts = createBodyParts(Seq(
      ("some", "1.zip", "content doesn't matter"),
      ("some", "2.txt", "Lorem ipsum dolor sit amet"),
    ))
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      body shouldBe "A multipart/form-data message contained a ZIP [1.zip] part but also other parts."
      status shouldBe BAD_REQUEST_400
      absoluteTarget.list.size shouldBe 0
    }
  }

  it should "report a malformed ZIP" in {
    val bodyParts = createBodyParts(Seq(("some", "1.zip", "invalid zip content")))
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (testDir / "drafts" / "foo" / uuid.toString / "bag/data" / relativeTarget).createDirectories()
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      absoluteTarget.list.size shouldBe 0
      status shouldBe BAD_REQUEST_400
      body shouldBe s"ZIP file is malformed. No entries found."
    }
  }

  it should "extract all files from a ZIP" in {
    File("src/test/resources/manual-test/Archive.zip").copyTo(testDir / "input" / "1.zip")
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val bagDir = testDir / "drafts" / "foo" / uuid.toString / "bag"
    val absoluteTarget = (bagDir / "data" / relativeTarget).createDirectories()
    absoluteTarget.list.size shouldBe 0 // precondition
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = Seq(("formFieldName", (testDir / "input/1.zip").toJava))
    ) {
      body shouldBe ""
      status shouldBe OK_200
      absoluteTarget.walk().map(_.name).toList should contain theSameElementsAs List(
        "dir", "login.html", "readme.md", "__MACOSX", "._login.html", "upload.html"
      )
      (bagDir / "manifest-sha1.txt")
        .lines
        .map(_.replaceAll(".* +", "")) should contain theSameElementsAs List(
        "login.html", "readme.md", "__MACOSX/._login.html", "upload.html"
      ).map("data/path/to/dir/" + _)

    }
  }

  it should "report a missing content disposition" in {
    val uuid = createDeposit

    // upload a file
    post(
      uri = s"/deposit/$uuid/file/path/to/",
      headers = Seq(fooBarBasicAuthHeader),
      body = "Lorem ipsum dolor sit amet"
    ) {
      status shouldBe BAD_REQUEST_400
      body shouldBe """Must have a Content-Type starting with "multipart/", got None."""
    }
  }

  it should "report an invalid content disposition" in {
    val uuid = createDataset

    // upload a file
    post(
      uri = s"/deposit/$uuid/file/path/to/",
      headers = Seq(fooBarBasicAuthHeader, ("Content-Type", "text/plain")),
      body = "Lorem ipsum dolor sit amet"
    ) {
      status shouldBe BAD_REQUEST_400
      body shouldBe """Must have a Content-Type starting with "multipart/", got Some(text/plain)."""
    }
  }

  s"PUT" should "return 201 for a new respectively 200 for a replaced file" in {
    val uuid = createDeposit

    val bagBase = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid.toString)).getDataFiles.get.bag
    val shortContent = "Lorum ipsum"
    val longContent = "dolor sit amet"

    // first upload
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(fooBarBasicAuthHeader),
      body = longContent
    ) {
      status shouldBe CREATED_201
      (bagBase / "data/path/to/text.txt").contentAsString shouldBe longContent
      (bagBase / "manifest-sha1.txt").contentAsString shouldBe "0d21d1af59b36f0bee70fd034e931ec72f04f1cd  data/path/to/text.txt\n"
    }

    // second upload of same file
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(fooBarBasicAuthHeader),
      body = shortContent
    ) {
      status shouldBe OK_200
      (bagBase / "data/path/to/text.txt").contentAsString shouldBe shortContent
      (bagBase / "manifest-sha1.txt").contentAsString shouldBe "c5b8de8cc3587aef4e118a481115391033621e06  data/path/to/text.txt\n"
    }
  }

  private def createBodyParts(files: Seq[(String, String, String)]): Seq[(String, java.io.File)] = {
    bodyParts(testDir / "input", files)
  }

  private def createDataset: UUID = {
    val responseBody = post(
      uri = s"/deposit",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      new String(bodyBytes)
    }
    DepositInfo(responseBody)
      .map(_.id)
      .getOrRecover(e => fail(s"preparing a dataset failed: $e", e))
  }
}
