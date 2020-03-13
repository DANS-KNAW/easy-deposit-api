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
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalatest.Inspectors

class UploadSpec extends ServletFixture with Inspectors {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    (testDir / "drafts").createDirectories()
    (testDir / "input").createDirectory()
  }

  mountDepositServlet(
    createTestApp(),
    AuthenticationMocker.expectsUserFooBarAnyNumberOfTimes,
  )

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
      status shouldBe CREATED_201
    }
    val bagDir = bagDirOf(uuid)
    val uploaded = (uploadRootOf(bagDir) / relativeTarget).list
    uploaded.size shouldBe bodyParts.size
    forEvery(uploaded.toList) { file =>
      file.contentAsString shouldBe (testDir / "input" / file.name).contentAsString
    }
    (bagDir / "manifest-sha1.txt").lines.size shouldBe bodyParts.size
    (testDir / "staged").list.toList shouldBe empty
  }

  it should "refuse concurrent posts" in {
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
      ("some", "3.txt", "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"),
      ("more", "4.txt", "Ut enim ad minim veniam"),
    ))
    val uuid = createDeposit
    (testDir / s"staged/foo-$uuid-XYZ").createDirectories() // mocks a concurrent post
    post(
      uri = s"/deposit/$uuid/file/path/to/dir",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      status shouldBe CONFLICT_409
      body shouldBe "Another upload or submit is pending."
    }
    val bagDir = bagDirOf(uuid)
    (bagDir / "data").entries shouldBe empty
    (bagDir / "manifest-sha1.txt").lines shouldBe empty
    (testDir / "staged").entries.map(_.name).toList should contain only s"foo-$uuid-XYZ"
  }

  it should "report upload failure" in {
    // need at least two files to show lazy evaluation with the logging: the second file is not tried to process
    val bodyParts = createBodyParts(Seq(
      ("some", "1.txt", "Lorem ipsum dolor sit amet"),
      ("some", "2.txt", "consectetur adipiscing elit"),
    ))
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = uploadRootOf(bagDirOf(uuid)) / relativeTarget
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
    val absoluteTarget = (uploadRootOf(bagDirOf(uuid)) / relativeTarget).createDirectories()
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      body shouldBe "A multipart/form-data message contained an archive part [2.zip] but also other parts."
      status shouldBe BAD_REQUEST_400
    }
    absoluteTarget.entries shouldBe empty // preceding plain file not added to draft bag
  }

  it should "report a ZIP item found without uploading any of the others" in {
    val bodyParts = createBodyParts(Seq(
      ("some", "1.zip", "content doesn't matter"),
      ("some", "2.txt", "Lorem ipsum dolor sit amet"),
    ))
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (uploadRootOf(bagDirOf(uuid)) / relativeTarget).createDirectories()
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      body shouldBe "A multipart/form-data message contained an archive part [1.zip] but also other parts."
      status shouldBe BAD_REQUEST_400
    }
    absoluteTarget.entries shouldBe empty
  }

  it should "escape file name in error message" in {
    val bodyParts = createBodyParts(Seq(("some", "1<x.zip", "invalid zip content")))
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (uploadRootOf(bagDirOf(uuid)) / relativeTarget).createDirectories()
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = bodyParts
    ) {
      status shouldBe BAD_REQUEST_400
      body shouldBe "Can't extract file(s) from 1&lt;x.zip into path/to/dir/. No entries found."
    }
    absoluteTarget.entries shouldBe empty
  }

  it should "extract the files of a zip into the relative target" in {
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val bagDir = bagDirOf(uuid)
    val absoluteTarget = (uploadRootOf(bagDir) / relativeTarget).createDirectories()
    absoluteTarget.entries shouldBe empty // precondition
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget", // another post-URI tested with the same zip
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = Seq(("formFieldName", File("src/test/resources/manual-test/macosx.zip").toJava))
    ) {
      body shouldBe ""
      status shouldBe CREATED_201
    }
    absoluteTarget.walk().map(_.name).toList should contain theSameElementsAs List(
      "dir", "login.html", "readme.md", "upload.html"
    )
    (bagDir / "manifest-sha1.txt")
      .lines
      .map(_.replaceAll(".* +", "")) should contain theSameElementsAs List(
      "login.html", "readme.md", "upload.html"
    ).map(s"data/original/$relativeTarget/" + _)

    // get should show uploaded files
    get(
      uri = s"/deposit/$uuid/file/",
      headers = Seq(fooBarBasicAuthHeader),
    ) {
      status shouldBe OK_200
      body should include("""{"filename":"readme.md","dirpath":"path/to/dir",""")
      body should include("""{"filename":"upload.html","dirpath":"path/to/dir",""")
      body should include("""{"filename":"login.html","dirpath":"path/to/dir",""")
    }
  }

  it should "extract the files of a tar into the relative target" in {
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val absoluteTarget = (uploadRootOf(bagDirOf(uuid)) / relativeTarget).createDirectories()
    absoluteTarget.entries shouldBe empty // precondition
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget", // another post-URI tested with the same zip
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = Seq(("formFieldName", File("src/test/resources/manual-test/ruimtereis-bag.tar").toJava))
    ) {
      body shouldBe ""
      status shouldBe CREATED_201
    }
    // more extensive verifications for the zip test
    absoluteTarget.walk().map(_.name).toList should contain("ruimtereis01_verklaring.txt")
  }

  it should "extract all files from a ZIP, with a nested zip" in {
    val uuid = createDeposit
    val relativeTarget = "path/to/dir"
    val bagDir = bagDirOf(uuid)
    val absoluteTarget = (uploadRootOf(bagDir) / relativeTarget).createDirectories()
    absoluteTarget.entries shouldBe empty // precondition
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = Seq("formFieldName" -> File("src/test/resources/manual-test/nested.zip").toJava)
    ) {
      body shouldBe empty
      status shouldBe CREATED_201
    }
    absoluteTarget.walk().map(_.name).toList should contain theSameElementsAs List(
      "dir", "myCompress", ".DS_Store", "secondLayer", "test.txt", "test_file.txt", "deeper.zip"
    )
    (bagDir / "manifest-sha1.txt")
      .lines
      .map(_.replaceAll(".* +", "")) should contain theSameElementsAs List(
      "myCompress/test_file.txt", "myCompress/.DS_Store", "myCompress/secondLayer/test.txt", "myCompress/deeper.zip"
    ).map("data/original/path/to/dir/" + _)

    // get should show uploaded files
    get(
      uri = s"/deposit/$uuid/file/",
      headers = Seq(fooBarBasicAuthHeader),
    ) {
      status shouldBe OK_200
      body should include("""{"filename":"deeper.zip","dirpath":"path/to/dir/myCompress",""")
      body should include("""{"filename":"test_file.txt","dirpath":"path/to/dir/myCompress",""")
      body should include("""{"filename":".DS_Store","dirpath":"path/to/dir/myCompress",""")
      body should include("""{"filename":"test.txt","dirpath":"path/to/dir/myCompress/secondLayer",""")
    }
  }

  it should "extract ZIP to root of data dir in the bag" in {
    val uuid = createDeposit
    val bagDir = bagDirOf(uuid)
    val absoluteTarget = uploadRootOf(bagDir).createDirectories()
    absoluteTarget.entries shouldBe empty // precondition
    post(
      uri = s"/deposit/$uuid/file/", // another post-URI tested with the same zip
      params = Iterable(),
      headers = Seq(fooBarBasicAuthHeader),
      files = Seq(("formFieldName", File("src/test/resources/manual-test/macosx.zip").toJava))
    ) {
      body shouldBe ""
      status shouldBe CREATED_201
    }
    absoluteTarget.walk().map(_.name).toList should contain theSameElementsAs List(
      "original", "login.html", "readme.md", "upload.html"
    )
    (bagDir / "manifest-sha1.txt")
      .lines
      .map(_.replaceAll(".* +", "")) should contain theSameElementsAs List(
      "login.html", "readme.md", "upload.html"
    ).map(s"data/original/" + _)

    // get should show uploaded files
    get(
      uri = s"/deposit/$uuid/file/",
      headers = Seq(fooBarBasicAuthHeader),
    ) {
      status shouldBe OK_200
      body should include("""{"filename":"readme.md","dirpath":"",""")
      body should include("""{"filename":"upload.html","dirpath":"",""")
      body should include("""{"filename":"login.html","dirpath":"",""")
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
      body shouldBe """Content-Type is a mandatory request header and must start with "multipart/"."""
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
      body shouldBe """Content-Type must start with "multipart/". Got: text/plain"""
    }
  }

  "PUT" should "return 201 for a new respectively 204 for a replaced file" in {
    val uuid = createDeposit

    val bagBase = bagDirOf(uuid)
    val shortContent = "Lorum ipsum"
    val longContent = "dolor sit amet"

    // first upload
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(fooBarBasicAuthHeader, contentTypePlainText),
      body = longContent
    ) {
      status shouldBe CREATED_201
    }
    (bagBase / "data/original/path/to/text.txt").contentAsString shouldBe longContent
    (bagBase / "manifest-sha1.txt").contentAsString shouldBe "0d21d1af59b36f0bee70fd034e931ec72f04f1cd  data/original/path/to/text.txt\n"

    // second upload of same file
    val sha = "c5b8de8cc3587aef4e118a481115391033621e06"
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(fooBarBasicAuthHeader, contentTypePlainText),
      body = shortContent
    ) {
      status shouldBe NO_CONTENT_204
    }
    (bagBase / "data/original/path/to/text.txt").contentAsString shouldBe shortContent
    (bagBase / "manifest-sha1.txt").contentAsString shouldBe s"$sha  data/original/path/to/text.txt\n"

    // get should show uploaded file once
    get(
      uri = s"/deposit/$uuid/file/path",
      headers = Seq(fooBarBasicAuthHeader),
    ) {
      status shouldBe OK_200
      body shouldBe s"""[{"filename":"text.txt","dirpath":"path/to","sha1sum":"$sha"}]"""
    }
  }

  it should "upload file to root of data folder" in {
    val uuid = createDeposit

    val bagBase = bagDirOf(uuid)
    val shortContent = "Lorum ipsum"
    val sha = "c5b8de8cc3587aef4e118a481115391033621e06"
    put(
      uri = s"/deposit/$uuid/file/text.txt",
      headers = Seq(fooBarBasicAuthHeader, contentTypePlainText),
      body = shortContent
    ) {
      status shouldBe CREATED_201
    }
    (bagBase / "data/original/text.txt").contentAsString shouldBe shortContent
    (bagBase / "manifest-sha1.txt").contentAsString shouldBe s"$sha  data/original/text.txt\n"

    // get should show uploaded file once
    get(
      uri = s"/deposit/$uuid/file/",
      headers = Seq(fooBarBasicAuthHeader),
    ) {
      status shouldBe OK_200
      body shouldBe s"""[{"filename":"text.txt","dirpath":"","sha1sum":"$sha"}]"""
    }
  }

  private def bagDirOf(uuid: String) = testDir / "drafts" / "foo" / uuid / bagDirName

  private def uploadRootOf(bagDir: File) = bagDir / "data" / "original"

  private def createBodyParts(files: Seq[(String, String, String)]): Seq[(String, java.io.File)] = {
    bodyParts(testDir / "input", files)
  }

  private def createDataset: UUID = {
    val responseBody = post(
      uri = "/deposit",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body
    }
    DepositInfo(responseBody)
      .map(_.id)
      .getOrRecover(e => fail(s"preparing a dataset failed: $e", e))
  }
}
