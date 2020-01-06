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

import better.files.File
import javax.servlet.http.Part
import nl.knaw.dans.easy.deposit.Errors.ArchiveMustBeOnlyFileException
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import org.scalatra.servlet.{ FileItem, MultipartConfig }

import scala.util.{ Failure, Success }

class RichFileItemsSpec extends TestSupportFixture with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "nextAsZipIfOnlyOne" should "return nothing" in {
    val fileItems = Iterator[FileItem]().buffered
    fileItems.nextAsArchiveIfOnlyOne shouldBe Success(None)
    fileItems.hasNext shouldBe false
  }

  it should "return the zip item between form fields without selected files" in {
    val fileItems = Iterator(
      mockFileItem(""),
      mockFileItem("some.zip"),
      mockFileItem(""),
    ).buffered
    fileItems.nextAsArchiveIfOnlyOne should matchPattern { case Success(Some(_)) => }
    fileItems.hasNext shouldBe false
  }

  it should "recognize a zip by all possible extensions and mime-types" in {
    val fileItems: Seq[FileItem] = Seq(
      "zip",
      "gzip",
      "z",
      "gz"
    ).map(ext => mockFileItem(s"some.$ext")) ++ Seq(
      "application/zip",
      "application/gzip",
      "application/x-compress",
      "application/x-compressed",
      "application/x-zip-compress",
      "application/x-zip-compressed",
      "application/x-gzip-compress",
      "application/x-gzip-compressed",
      "application/x-zip",
      "application/x-gzip",
    ).map(mockFileItem(s"some.thing", _))
    fileItems.map {
      Iterator(_)
        .buffered
        .nextAsArchiveIfOnlyOne
        .map(_.isDefined)
        .toString
    }.mkString("") shouldBe ("Success(true)" * fileItems.size)
  }

  it should "refuse a zip item if there are more items" in {
    val fileItems = Iterator(
      mockFileItem(""),
      mockFileItem("some.zip"),
      mockFileItem(""),
      mockFileItem("other.zip"),
      mockFileItem(""),
    ).buffered
    fileItems.nextAsArchiveIfOnlyOne should matchPattern {
      case Failure(e: ArchiveMustBeOnlyFileException) if e.getMessage ==
        "A multipart/form-data message contained an archive part [some.zip] but also other parts." =>
    }
  }

  "moveNonZips" should "have no problems with an empty list" in {
    val fileItems = Iterator(
      mockFileItem(""),
    ).buffered

    createMultipartConfig.moveNonArchive(fileItems, createStageDir, UUID.randomUUID()) shouldBe a[Success[_]]
    (testDir / "staged").list should have size 0
  }

  it should "refuse a zip" in {
    val contentOfStagedUpload = "Lorem ipsum est"
    val fileItems = Iterator(
      mockFileItem("some.txt", content = contentOfStagedUpload),
      mockFileItem("some.zip"),
      mockFileItem("another.txt", content = "Dolor sit amet"),
    ).buffered

    createMultipartConfig.moveNonArchive(fileItems, createStageDir, UUID.randomUUID()) should matchPattern {
      case Failure(e: ArchiveMustBeOnlyFileException) if e.getMessage ==
        "A multipart/form-data message contained an archive part [some.zip] but also other parts." =>
      // moveNonZips should not have been called at all by the servlet with these items
      // that's out of scope for this unit test but is the (fall back) rationale for the "but ..."
      // of the message returned to the client
    }
    (testDir / "staged" / "some.txt").contentAsString shouldBe contentOfStagedUpload
    (testDir / "staged").list should have size 1 // the file after the zip is not processed
  }

  private val multipartLocation: File = testDir / "mulitpart-location"

  private def createMultipartConfig = {
    val dir = multipartLocation.createDirectories()
    MultipartConfig(Some(dir.toString()), None, None, Option(5))
  }

  private def createStageDir = {
    (testDir / "staged").createDirectories()
  }

  private def mockFileItem(fileName: String, contentType: String = null, content: String = "") = {
    val mocked = mock[Part]
    (() => mocked.getSize) expects() returning 20 anyNumberOfTimes()
    (() => mocked.getName) expects() returning "formFieldName" anyNumberOfTimes()
    mocked.getHeader _ expects "content-disposition" returning "filename=" + fileName anyNumberOfTimes()
    mocked.getHeader _ expects "content-type" returning contentType anyNumberOfTimes()
    (() => mocked.getContentType) expects() returning contentType anyNumberOfTimes()
    mocked.write _ expects *  anyNumberOfTimes() onCall { f: String =>
      (multipartLocation / f).write(content)
      ()
    }
    FileItem(mocked)
  }
}
