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

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import javax.servlet.http.Part
import nl.knaw.dans.easy.deposit.{ TestSupportFixture, ZipMustBeOnlyFileException }
import org.scalamock.scalatest.MockFactory
import org.scalatra.servlet.FileItem

import scala.util.{ Failure, Success }

class RichFileItemsSpec extends TestSupportFixture with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "nextAsZipIfOnlyOne" should "return nothing" in {
    val fileItems = Iterator[FileItem]().buffered
    fileItems.nextAsZipIfOnlyOne shouldBe Success(None)
    fileItems.hasNext shouldBe false
  }

  it should "return the zip item between form fields without selected files" in {
    val fileItems = Iterator(
      mockFileItem(""),
      mockFileItem("some.zip"),
      mockFileItem(""),
    ).buffered
    fileItems.nextAsZipIfOnlyOne should matchPattern { case Success(Some(_)) => }
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
        .nextAsZipIfOnlyOne
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
    fileItems.nextAsZipIfOnlyOne should matchPattern {
      case Failure(ZipMustBeOnlyFileException("some.zip")) =>
    }
  }

  "copyPlainItemsTo" should "copy the plain item between form fields without selected files" in {
    val stagingDir = (testDir / "staging").createDirectories()
    val fileItems = Iterator(
      mockFileItem(""),
      mockFileItem("some.txt"),
      mockFileItem(""),
      mockFileItem("more.txt"),
      mockFileItem(""),
    ).buffered
    fileItems.nextAsZipIfOnlyOne shouldBe Success(None)
    fileItems.copyPlainItemsTo(stagingDir) shouldBe Success(())
    stagingDir.walk().map(_.name).toList should
      contain theSameElementsAs List("staging", "some.txt", "more.txt")
  }

  it should "refuse to copy a zip as plain item" in {
    val stagingDir = (testDir / "staging").createDirectories()
    val fileItems = Iterator(
      mockFileItem("some.txt"),
      mockFileItem("other.zip"),
    ).buffered
    fileItems.nextAsZipIfOnlyOne shouldBe Success(None)
    fileItems.copyPlainItemsTo(stagingDir) should matchPattern {
      case Failure(ZipMustBeOnlyFileException("other.zip")) =>
    }
    stagingDir.walk().map(_.name).toList should
      contain theSameElementsAs List("staging", "some.txt")
  }

  private def mockFileItem(fileName: String, contentType: String = null) = {
    val mocked = mock[Part]
    (() => mocked.getSize) expects() returning 20 anyNumberOfTimes()
    (() => mocked.getName) expects() returning "formFieldName" anyNumberOfTimes()
    mocked.getHeader _ expects "content-disposition" returning "filename=" + fileName anyNumberOfTimes()
    mocked.getHeader _ expects "content-type" returning contentType anyNumberOfTimes()
    (() => mocked.getContentType) expects() returning contentType anyNumberOfTimes()
    (() => mocked.getInputStream) expects() returning new ByteArrayInputStream("Lorem ipsum est".getBytes(StandardCharsets.UTF_8)) anyNumberOfTimes()
    FileItem(mocked)
  }
}
