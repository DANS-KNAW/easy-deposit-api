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
package nl.knaw.dans.easy.deposit

import java.io.{ ByteArrayInputStream, InputStream }
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.zip.ZipInputStream

import better.files.File.temporaryDirectory
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.BadRequestException
import nl.knaw.dans.lib.error._

import scala.util.Failure

class StagedFilesSpec extends TestSupportFixture {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val draftsDir = testDir / "drafts"

  "unzip" should "report invalid content" in {
    val bag = DansV0Bag
      .empty(draftsDir / "bag").getOrRecover(e => fail(e.toString, e))
    bag.save().getOrRecover(e => fail(e.toString, e))
    val content = "Lorem ipsum est"
    val fileInBag = "location/in/data/dir/test.txt"
    val inputStream = new ZipInputStream(new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8)))

    temporaryDirectory().apply(
      StagedFiles(_, bag, Paths.get(fileInBag))
        .unzip(inputStream) should matchPattern {
        case Failure(BadRequestException(s)) if s == "ZIP file is malformed. No entries found." =>
      })
    inputStream.close()
  }
}
