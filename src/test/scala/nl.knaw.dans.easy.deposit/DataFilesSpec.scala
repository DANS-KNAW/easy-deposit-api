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
import java.nio.file.Paths

import scala.util.Success

class DataFilesSpec extends TestSupportFixture {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    draftsDir.createDirectories()
  }

  private val draftsDir = testDir / "drafts"

  "write" should "blabla" in {
    // preparations
    val dd = DepositDir(draftsDir, "foo", uuid)
    val content = "Lorum ipsum est"
    val inputStream: InputStream = new ByteArrayInputStream(content.getBytes())
    val fileInBag = "test.txt"

    // execution
    dd.getDataFiles.map(_.write(inputStream, Paths.get(fileInBag))) shouldBe Success(true)

    // verifications
    (dd.baseDir / "bag" / "data" / fileInBag).contentAsString shouldBe content
    (dd.baseDir / "bag" / "???").contentAsString shouldBe // TODO fill in ???
      s"""{"fileName": "$fileInBag", "dirPath": "???", "sha1sum": "???"}"""
  }
}
