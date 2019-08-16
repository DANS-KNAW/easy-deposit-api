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
import java.nio.charset.StandardCharsets

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

  it should "extract all files from the zip" in {
    mockRichFileItemGetZipInputStream(new FileInputStream("src/test/resources/manual-test/macosx.zip")).apply(
      unzip(_) shouldBe Success(())
    )
    stagingDir.walk().map(_.name).toList should contain theSameElementsAs
      List("staging", "login.html", "readme.md", "upload.html")
  }

  it should "create parent directories not explicitly listed in the zip" in {
    // https://github.com/Davidhuangwei/TFM/
    mockRichFileItemGetZipInputStream(new FileInputStream("src/test/resources/manual-test/no-dir.zip")).apply(
      unzip(_) shouldBe Success(())
    )
    stagingDir.walk().map(_.name).toList should contain theSameElementsAs
    List("staging", "tfm.m", "FMC-copper-wiresFRD.png", "image_domain.m", "license.txt" /* 2-clause BSD */, "copper_wire_example.m")
  }

  it should "not throw ZipException: only DEFLATED entries can have EXT descriptor" in {
    mockRichFileItemGetZipInputStream(new FileInputStream("src/test/resources/manual-test/ruimtereis-bag.zip")).apply(
      unzip(_) shouldBe Success(())
    )
    (stagingDir / "data-test-2" / "data" / "ruimtereis01_verklaring.txt") should exist
    stagingDir.walk().map(_.name).toList should contain theSameElementsAs
      List(
        "staging", "data-test-2", "tagmanifest-md5.txt", "bagit.txt", "bag-info.txt", "manifest-sha1.txt",
        "data", "ruimtereis01_verklaring.txt", "secret.txt", "rand.2.txt", "metadata", "dataset.xml", "files.xml"
      )
  }

  private def unzip(stream: ZipArchiveInputStream): Try[Unit] = {
    stream.unzipPlainEntriesTo(stagingDir.createDirectories())
  }

  private def mockRichFileItemGetZipInputStream(inputStream: InputStream): ManagedResource[ZipArchiveInputStream] = {
    /*
     The next example would use java.util throwing the tested ZipException, we use apache.commons

     import better.files._
     inputStream.asZipInputStream
     */
    managed(new ZipArchiveInputStream(inputStream, "UTF8", true, true))
  }
}
