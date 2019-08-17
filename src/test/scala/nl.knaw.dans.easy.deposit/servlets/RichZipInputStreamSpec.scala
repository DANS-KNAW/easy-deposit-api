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

import java.io.{ ByteArrayInputStream, FileInputStream, InputStream }
import java.nio.charset.{ Charset, StandardCharsets }

import better.files.{ File, UnicodeCharset, _ }
import nl.knaw.dans.easy.deposit.Errors.MalformedZipException
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import resource.{ ManagedResource, managed }

import scala.util.{ Failure, Success, Try }

class RichZipInputStreamSpec extends TestSupportFixture {

  private val stagingDir = testDir / "staging"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "unzipPlainEntriesTo" should "report invalid content" in {
    mockRichFileItemGetZipInputStream(new ByteArrayInputStream("Lorem ipsum est".getBytes(StandardCharsets.UTF_8))).apply(
      unzip(_) shouldBe Failure(MalformedZipException("No entries found."))
    )
  }

  it should "extract allowed files from a zip" in {
    val zipFile = "src/test/resources/manual-test/macosx.zip"
    val notExpected = List("__MACOSX/", "__MACOSX/._login.html")
    val expected = List("login.html", "readme.md", "upload.html")
    entriesOf(zipFile) should contain theSameElementsAs expected ::: notExpected
    testUnzipPlainEntries(zipFile, expected, notExpected)
  }

  it should "create parent directories not explicitly listed in the zip" in {
    // https://github.com/Davidhuangwei/TFM/
    val zipFile = "src/test/resources/manual-test/no-dir.zip"
    val notExpected = List("__MACOSX/._copper_wire_example.m", "__MACOSX/._image_domain.m", "__MACOSX/._tfm.m")
    val expected = List("tfm.m", "FMC-copper-wiresFRD.png", "image_domain.m", "license.txt" /* 2-clause BSD */ , "copper_wire_example.m")
    entriesOf(zipFile) should contain theSameElementsAs expected ::: notExpected
    testUnzipPlainEntries(zipFile, expected, notExpected)
  }

  it should "not throw ZipException: only DEFLATED entries can have EXT descriptor" in {
    val zipFile = "src/test/resources/manual-test/ruimtereis-bag.zip"
    val expected = List(
     "data-test-2", "tagmanifest-md5.txt", "bagit.txt", "bag-info.txt", "manifest-sha1.txt",
      "data", "ruimtereis01_verklaring.txt", "secret.txt", "rand.2.txt", "metadata", "dataset.xml", "files.xml"
    )
    // using java.util.zip.ZipInputStream
    Try {entriesOf(zipFile)} shouldBe a[Failure[_]] // TODO check the DEFLATED message

    // using org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
    testUnzipPlainEntries(zipFile, expected, List[String]())
    (stagingDir / "data-test-2" / "data" / "ruimtereis01_verklaring.txt") should exist
  }

  private def testUnzipPlainEntries(zipFile: String, expected: List[String], notExpected: List[String]): Any = {
    mockRichFileItemGetZipInputStream(new FileInputStream(zipFile)).apply(
      unzip(_) shouldBe Success(())
    )
    val actual = stagingDir.walk().map(_.name).toList
    actual should contain theSameElementsAs expected :+ "staging"
    actual shouldNot contain theSameElementsAs notExpected
  }

  /**
   * Note that not all zips supported by ZipArchiveInputStream (used by the application)
   * are supported by newZipInputStream used to test preconditions.
   */
  private def entriesOf(zipFile: String) = {
    val charset: Charset = UnicodeCharset(Charset.defaultCharset()) // TODO as implicit?
    File(zipFile).newZipInputStream(charset).mapEntries(_.getName).toList
  }

  private def unzip(stream: ZipArchiveInputStream): Try[Unit] = {
    stream.unzipPlainEntriesTo(stagingDir.createDirectories())
  }

  /** Mocks how a file item of a http request is processed by the application */
  private def mockRichFileItemGetZipInputStream(inputStream: InputStream): ManagedResource[ZipArchiveInputStream] = {
    /*
     The next example would use java.util throwing the tested ZipException, we use apache.commons

     import better.files._
     inputStream.asZipInputStream
     */
    managed(new ZipArchiveInputStream(inputStream, "UTF8", true, true))
  }
}
