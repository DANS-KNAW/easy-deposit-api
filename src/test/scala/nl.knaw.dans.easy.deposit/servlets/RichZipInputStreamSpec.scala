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
import java.util.zip.ZipInputStream

import better.files.File
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.BadRequestException
import resource.managed

import scala.util.{ Failure, Success }

class RichZipInputStreamSpec extends TestSupportFixture {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "unzipPlainEntriesTo" should "report invalid content" in {
    managed(new ZipInputStream(new ByteArrayInputStream("Lorem ipsum est".getBytes(StandardCharsets.UTF_8)))).apply(
      _.unzipPlainEntriesTo(testDir / "dummy") should matchPattern {
        case Failure(BadRequestException(s)) if s == "ZIP file is malformed. No entries found." =>
      })
  }

  it should "extract all files from the zip" in {
    val stagingDir = testDir / "staging"
    managed(File("src/test/resources/manual-test/Archive.zip").newZipInputStream).apply(
      _.unzipPlainEntriesTo(stagingDir.createDirectories()) shouldBe Success(()))
    stagingDir.walk().map(_.name).toList should contain theSameElementsAs
      List("staging", "login.html", "readme.md", "__MACOSX", "._login.html", "upload.html")
  }
}
