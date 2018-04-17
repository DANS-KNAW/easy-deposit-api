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

import scala.util.Success

class DataFilesSpec extends TestSupportFixture {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    draftsDir.createDirectories()
  }

  private val draftsDir = testDir / "drafts"

  "write" should "write content to the path specified" in {
    val dd = DepositDir(draftsDir, "user01", uuid)
    val dataFiles = dd.getDataFiles.get
    val content = "Lorem ipsum est"
    val inputStream: InputStream = new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))
    val fileInBag = "location/in/data/dir/test.txt"

    dataFiles.write(inputStream, Paths.get(fileInBag)) shouldBe Success(true)

    (draftsDir / "user01" / uuid.toString / "bag" / "data" /  fileInBag).contentAsString shouldBe content
  }
}
