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

import better.files.StringOps
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.Errors.ConflictException
import nl.knaw.dans.lib.error._

import scala.util.{ Failure, Success }

class StagedFilesTargetSpec extends TestSupportFixture {

  private val draftDir = testDir / "draft"
  private val stagedDir = testDir / "stage-for-submit"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    stagedDir.createDirectories()
  }

  "moveAllFrom" should "add payload files" in {
    (stagedDir / "sub/path").createDirectories()
    (stagedDir / "sub/path/some.thing").createFile().write("new content")
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = newEmptyBag
    bag.save()
    bag.data.list.size shouldBe 0
    bag.fetchFiles.size shouldBe 0

    StagedFilesTarget(bag, Paths.get("path/to"))
      .moveAllFrom(stagedDir) shouldBe Success(())

    val newBag = DansV0Bag.read(draftDir).getOrRecover(e => fail(e))
    (newBag.data / "path/to/some.thing").contentAsString shouldBe "new content"
    (newBag.data / "path/to/sub/path/some.thing").contentAsString shouldBe "new content"
    newBag.fetchFiles.size shouldBe 0
  }

  it should "add a payload file to the root of the data folder" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = newEmptyBag
    bag.save()

    StagedFilesTarget(bag, Paths.get(""))
      .moveAllFrom(stagedDir) shouldBe Success(())

    val newBag = readDraftBag
    (newBag.data / "some.thing").contentAsString shouldBe "new content"
    newBag.fetchFiles.size shouldBe 0
  }

  it should "not replace a fetch file" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val url = new URL("https://raw.githubusercontent.com/DANS-KNAW/easy-deposit-api/master/README.md")
    val bag = newEmptyBag.addFetchItem(
      url,
      Paths.get("path/to/some.thing"),
    ).getOrRecover { e =>
      assume(!e.isInstanceOf[UnknownHostException])
      fail(e)
    }
    bag.save()
    bag.data.list.size shouldBe 0
    bag.fetchFiles.size shouldBe 1

    StagedFilesTarget(bag, Paths.get("path/to"))
      .moveAllFrom(stagedDir) should matchPattern {
      case Failure(e: ConflictException) if e.getMessage == "The following file(s) already exist on the server: path/to/some.thing" =>
    }

    val newBag = readDraftBag
    newBag.data.list.size shouldBe 0
    newBag.fetchFiles.size shouldBe 1
  }

  it should "not replace a payload file" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = newEmptyBag.addPayloadFile("Lorum ipsum".inputStream, Paths.get("path/to/some.thing")).getOrRecover(e => fail(e))
    bag.save()
    (bag.data / "path/to/some.thing").contentAsString shouldBe "Lorum ipsum"

    StagedFilesTarget(bag, Paths.get("path/to"))
      .moveAllFrom(stagedDir) should matchPattern {
      case Failure(e: ConflictException) if e.getMessage == "The following file(s) already exist on the server: path/to/some.thing" =>
    }

    val newBag = readDraftBag
    (newBag.data / "path/to/some.thing").contentAsString shouldBe "Lorum ipsum"
    newBag.fetchFiles.size shouldBe 0
  }

  // TODO a test that would log the following line:
  //  https://github.com/DANS-KNAW/easy-deposit-api/blob/748c663d/src/main/scala/nl.knaw.dans.easy.deposit/StagedFilesTarget.scala#L69
  //  Mock a FileAlreadyExistsException by one of the addPayloadFile calls, that mocks a PUT request while processing a POST.
  //  Preceding and subsequent files are expected to be added to the draft bag.
  //  Without the recoverWith the request would return some error but still might have added some files without saving the sha's in the manifests.

  private def newEmptyBag = {
    DansV0Bag.empty(draftDir).getOrRecover(e => fail(e))
  }

  private def readDraftBag = {
    DansV0Bag.read(draftDir).getOrRecover(e => fail(e))
  }
}
