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
import java.nio.file.Paths
import java.util.zip.ZipException

import better.files.{ File, UnicodeCharset, _ }
import nl.knaw.dans.easy.deposit.Errors.MalformedArchiveException
import nl.knaw.dans.easy.deposit.{ DepositDir, TestSupportFixture }
import org.apache.commons.compress.archivers.ArchiveInputStream
import org.apache.commons.compress.archivers.tar.{ TarArchiveInputStream, TarConstants }
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import resource.managed

import scala.util.{ Failure, Success, Try }

class RichArchiveInputStreamSpec extends TestSupportFixture {

  private val stagingDir = testDir / "staging"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "unpackPlainEntriesTo" should "report invalid content" in {
    unpackZip(new ByteArrayInputStream("Lorem ipsum est".getBytes(StandardCharsets.UTF_8))) should matchPattern {
      case Failure(e: MalformedArchiveException) if e.getMessage ==
        "Can't extract file(s) from uploaded.filename into some/path/. No entries found." =>
    }
    stagingDir.entries shouldBe empty
  }

  it should "complain about an invalid zip" in {
    unpackZip(new FileInputStream(s"src/test/resources/manual-test/invalid.zip")) should matchPattern {
      case Failure(e: MalformedArchiveException) if e.getMessage ==
        "Can't extract file(s) from uploaded.filename into some/path/. No entries found." =>
    }
    stagingDir.entries shouldBe empty
  }

  it should "complain about an invalid item" in {
    unpackZip(new FileInputStream(s"src/test/resources/manual-test/bla-stored-dd-contradicts-actualsize.zip")) should matchPattern {
      case Failure(e: MalformedArchiveException) if e.getMessage
        .matches("Can't extract test1.xml from uploaded.filename into some/path/. actual and claimed size don't match.*See http.*") =>
    }
    stagingDir.entries.toList should have size 1
    (stagingDir / "test1.xml").toJava should have length 0
  }

  it should "complain about a zip trying to put files outside the intended target" in {
    unpackZip(new FileInputStream(s"src/test/resources/manual-test/slip.zip")) should matchPattern {
      case Failure(e: MalformedArchiveException) if e.getMessage == "" +
        "Can't extract ../../user001washere.txt from uploaded.filename into some/path/. Invalid path" =>
    }
    stagingDir.entries shouldBe empty
    testDir.entries should have size 1
    testDir.parent.entries.filter(!_.name.endsWith("Spec")) shouldBe empty
  }

  it should "complain about an empty zip" in {
    unpackZip(new FileInputStream(s"src/test/resources/manual-test/empty.zip")) should matchPattern {
      case Failure(e: MalformedArchiveException) if e.getMessage ==
        "Can't extract file(s) from uploaded.filename into some/path/. No entries found." =>
    }
    stagingDir.entries shouldBe empty
  }

  it should "extract files from a nested zip" in {
    unpackZip(new FileInputStream(s"src/test/resources/manual-test/nested.zip")) shouldBe Success(())
    // further details checked in uploadSpec
  }

  it should "complain about a not supported zip" in {
    // note that the file intentionally has the wrong extension. It content looks like nested.zip
    // Apparently a different zip-format, see also https://issues.apache.org/jira/browse/COMPRESS-480
    unpackZip(new FileInputStream(s"src/test/resources/manual-test/invalid.tar.gz")) should matchPattern {
      case Failure(e: MalformedArchiveException) if e.getMessage ==
        "Can't extract file(s) from uploaded.filename into some/path/. Unexpected record signature: 0X88B1F" =>
    }
    stagingDir.entries shouldBe empty
  }

  it should "complain about a zip with tar.gz extension" in {
    unpackTar(new FileInputStream(s"src/test/resources/manual-test/invalid.tar.gz")) should matchPattern {
      // the cause in the message has unprintable characters
      // might cause the failure of: e.getMessage.matches(".*Please <a href=.*>contact DANS</a>.*")
      case Failure(e: MalformedArchiveException) if e.getMessage
        .endsWith(">contact DANS</a>") && e.getMessage
        .startsWith("Can't extract file(s) from uploaded.filename into some/path/. Invalid byte 73 at offset 0 in 'I???D??????.' len=12. Please <a href=") =>
      // compare https://github.com/DANS-KNAW/easy-deposit-api/blob/296c615b/src/main/scala/nl.knaw.dans.easy.deposit/servlets/package.scala#L105
      // with the stack trace for a too large archive reported by https://drivenbydata.atlassian.net/browse/EASY-2619
      // now recovering from IOException with cause IllegalArgumentException
    }
    stagingDir.entries shouldBe empty
  }

  it should "filter OS files from a zip" in {
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

    // precondition: test method `entriesOf` uses java.util.zip.ZipInputStream
    the[ZipException] thrownBy entriesOf(zipFile) should have message "only DEFLATED entries can have EXT descriptor"

    // implementation: uses org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
    unpackZip(new FileInputStream(zipFile)) shouldBe Success(())
    (stagingDir / "data-test-2" / "data" / "ruimtereis01_verklaring.txt") should exist
  }

  it should "handle tar" in {
    unpackTar(new FileInputStream(s"src/test/resources/manual-test/ruimtereis-bag.tar")) shouldBe Success(())
    (stagingDir / "data" / "ruimtereis01_verklaring.txt") should exist
  }

  it should "recursively remove empty directories" in {
    val zipFile = "src/test/resources/manual-test/empty-dir-win.zip"
    entriesOf(zipFile) should contain theSameElementsAs List(
      "empty-dir/parent1/",
      "empty-dir/parent1/parent2/",
      "empty-dir/parent1/parent2/empty-dir/",
      "empty-dir/parent1/sibling/",
      "empty-dir/parent1/sibling/hello.txt"
    )

    unpackZip(new FileInputStream(zipFile)) shouldBe Success(())
    (stagingDir / "empty-dir" / "parent1" / "sibling" / "hello.txt") should exist
    (stagingDir / "empty-dir" / "parent1" / "parent2") shouldNot exist
  }

  it should "remove empty directory" in {
    val zipFile = "src/test/resources/manual-test/empty-dir-mac.zip"

    entriesOf(zipFile) should contain allElementsOf List(
      "empty-dir/parent1/parent2/empty-dir/",
      "empty-dir/parent1/sibling/hello.txt",
      "empty-dir/parent1/.DS_Store",
      // not interested in more of .DS_Store and __MACOSX/
    )

    unpackZip(new FileInputStream(zipFile)) shouldBe Success(())
    (stagingDir / "empty-dir" / "parent1" / "sibling" / "hello.txt") should exist
    (stagingDir / "empty-dir" / "parent1" / "parent2" / ".DS_Store") should exist
  }

  private def testUnzipPlainEntries(zipFile: String, expectedInStagingDir: List[String], notExpectedInStagingDir: List[String]): Any = {
    unpackZip(new FileInputStream(zipFile)) shouldBe Success(())
    val actual = stagingDir.walk().map(_.name).toList
    actual should contain theSameElementsAs expectedInStagingDir :+ "staging"
    actual shouldNot contain theSameElementsAs notExpectedInStagingDir
  }

  /**
   * Note that not all zips supported by ZipArchiveInputStream (used by the application)
   * are supported by newZipInputStream used to test preconditions.
   */
  private def entriesOf(zipFile: String) = {
    val charset: Charset = UnicodeCharset(Charset.defaultCharset()) // TODO as implicit?
    File(zipFile).newZipInputStream(charset).mapEntries(_.getName).toList
  }

  private def unpack(stream: ArchiveInputStream, fileName: String = "uploaded.filename"): Try[Unit] = {
    lazy val depositDir = DepositDir(testDir / "drafts", "user001", uuid)
    stream.unpackPlainEntriesTo(stagingDir.createDirectories(), depositDir, Paths.get("some/path"), fileName)
  }

  /** Mocks how a file item of a http request is processed by the application */
  private def unpackZip(inputStream: InputStream): Try[Unit] = {
    /*
     better.files.inputStream.asZipInputStream(inputStream)
     would use java.util throwing the tested ZipException, we use apache.commons
     */
    managed(new ZipArchiveInputStream(inputStream, "UTF8", true, true)).apply(unpack(_))
  }

  /** Mocks how a file item of a http request is processed by the application */
  private def unpackTar(inputStream: InputStream): Try[Unit] = {
    managed(new TarArchiveInputStream(inputStream, TarConstants.DEFAULT_BLKSIZE, TarConstants.DEFAULT_RCDSIZE, "UTF8", true))
      .apply(unpack(_))
  }
}
