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

import java.net.{ URL, UnknownHostException }
import java.nio.file.Paths
import java.util.UUID

import better.files.StringExtensions
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.lib.error._

import scala.util.Success

class StagedFilesSpec extends TestSupportFixture {

  private val draftDir = testDir / "draft"
  private val stagedDir = testDir / "staged"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    stagedDir.createDirectories()
  }

  "DataFiles.moveAll" should "add payload files" in {
    (stagedDir / "sub" / "path").createDirectories()
    (stagedDir / "sub" / "path" / "some.thing").createFile().write("new content")
    (stagedDir / "some.thing").createFile().write("more content")
    val bag = newEmptyBag
    bag.save()
    bag.data.list shouldBe empty
    bag.fetchFiles shouldBe empty

    DataFiles(bag, UUID.randomUUID()).moveAll(stagedDir, Paths.get("path/to")) shouldBe Success(())

    bag.fetchFiles shouldBe empty
    (bag.data / "original" / "path" / "to" / "some.thing").contentAsString shouldBe "more content"
    (bag.data / "original" / "path" / "to" / "sub" / "path" / "some.thing").contentAsString shouldBe "new content"
    stagedDir.walk().filter(!_.isDirectory) shouldBe empty
  }

  it should "add a payload file to the root of the data folder" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = newEmptyBag
    bag.save()

    DataFiles(bag, UUID.randomUUID()).moveAll(stagedDir, Paths.get("")) shouldBe Success(())

    (bag.data / "original" / "some.thing").contentAsString shouldBe "new content"
    bag.fetchFiles shouldBe empty
    stagedDir.walk().filter(!_.isDirectory) shouldBe empty
  }

  it should "replace a fetch file" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val url = new URL("https://raw.githubusercontent.com/DANS-KNAW/easy-deposit-api/master/README.md")
    val bag = newEmptyBag.addFetchItem(
      url,
      Paths.get("original/path/to/some.thing"),
    ).getOrRecover { e =>
      assume(!e.isInstanceOf[UnknownHostException])
      fail(e)
    }
    bag.save()
    bag.data.entries shouldBe empty
    bag.fetchFiles should not be empty

    DataFiles(bag, UUID.randomUUID()).moveAll(stagedDir, Paths.get("path/to")) shouldBe a[Success[_]]

    bag.data / "original" / "path" / "to" / "some.thing" should exist
    bag.fetchFiles shouldBe empty
    stagedDir.walk().filter(!_.isDirectory) shouldBe empty
  }

  it should "replace a payload file" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = newEmptyBag.addPayloadFile("Lorum ipsum".inputStream, Paths.get("original/path/to/some.thing")).getOrRecover(e => fail(e))
    bag.save()
    val target = bag.data / "original" / "path" / "to" / "some.thing"
    target.contentAsString shouldBe "Lorum ipsum" // pre condition

    DataFiles(bag, UUID.randomUUID()).moveAll(stagedDir, Paths.get("path/to")) shouldBe a[Success[_]]

    target.contentAsString shouldBe "new content" // post condition
    bag.fetchFiles shouldBe empty
    stagedDir.walk().filter(!_.isDirectory) shouldBe empty
  }

  private def newEmptyBag = {
    DansV0Bag.empty(draftDir).getOrRecover(e => fail(e))
  }
}
