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

import java.net.URL
import java.nio.file.Paths

import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.lib.error._

import scala.util.Success

class StagedFilesTargetSpec extends TestSupportFixture {

  private val draftDir = testDir / "draft"
  private val stagedDir = testDir / "staged"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    stagedDir.createDirectories()
  }

  "takeAllFrom" should "overwrite fetch file" in  {
    (stagedDir / "some.thing").createFile().write("new content")
    val url = new URL("https://raw.githubusercontent.com/DANS-KNAW/easy-deposit-api/master/README.md")
    val bag = DansV0Bag
      .empty(draftDir).getOrRecover(e => fail(e))
      .addFetchItem(url, Paths.get("path/to/some.thing")).getOrRecover(e => fail(e))
    bag.save()
    (bag.data / "path/to/some.thing").toJava shouldNot exist
    bag.fetchFiles.size shouldBe 1

    StagedFilesTarget(bag, Paths.get("path/to"))
        .takeAllFrom(stagedDir) shouldBe Success(())

    val newBag = DansV0Bag.read(draftDir).getOrRecover(e => fail(e))
    (newBag.data / "path/to/some.thing").contentAsString shouldBe "new content"
    newBag.fetchFiles.size shouldBe 0
  }
}
