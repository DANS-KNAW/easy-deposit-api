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

import java.io.{ FileInputStream, InputStream }
import java.nio.file.Paths

import scala.util.Success

class DataFilesSpec extends TestSupportFixture {

  before {
    clearTestDir()
    draftsDir.createDirectories()
  }
  private val draftsDir = testDir / "drafts"

  "write" should "blabla" in {
    // prepare empty deposit
    val dd = DepositDir(draftsDir, "foo", uuid)
    // prepare uploaded file
    val input = (draftsDir / "input" / "test.tst").write("Lorum ipsum est")
    val inputStream: InputStream = new FileInputStream(input.toJava)

    dd.getDataFiles.map(_.write(inputStream, Paths.get("test.txt"))) shouldBe Success(true)

    (dd.baseDir / "foo" / uuid.toString / "bag" / "data" / "test.txt")
      .contentAsString shouldBe "Lorum ipsum est"

    // TODO als verify the file meta data
  }
}
