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
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{ AccessDeniedException, Paths }
import java.util.UUID

import better.files.StringExtensions
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.Errors.NoSuchFileInDepositException
import nl.knaw.dans.easy.deposit.docs.FileInfo
import nl.knaw.dans.lib.error._

import scala.util.{ Failure, Success }

class DataFilesSpec extends TestSupportFixture {

  private val stagedDir = testDir / "staged"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    stagedDir.createDirectories()
  }

  "write" should "write content to the path specified" in {
    val bag = newEmptyBag
    val content = "Lorem ipsum est"
    val fileInBag = "location/in/data/dir/test.txt"

    DataFiles(save(bag), uuid)
      .write(content.inputStream, Paths.get(fileInBag)) shouldBe Success(true)

    (bag.data / "original" / fileInBag).contentAsString shouldBe content
  }

  it should "write content to the path specified of an old style bag" in {
    val bag = newEmptyBag
      .addPayloadFile(randomContent, Paths.get("1.txt")).getOrRecover(payloadFailure)
    val fileInBag = "test.txt"

    DataFiles(save(bag), uuid)
      .write(randomContent, Paths.get(fileInBag)) shouldBe Success(true)

    (bag.data / fileInBag) should exist // N.B: without "original" of the previous test
  }

  it should "use bag.data as upload root when 'original' has siblings" in {
    val bag = newEmptyBag
      .addPayloadFile(randomContent, Paths.get("1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/2.txt")).getOrRecover(payloadFailure)
    val fileInBag = "test.txt"

    DataFiles(save(bag), uuid)
      .write(randomContent, Paths.get(fileInBag)) shouldBe Success(true)

    (bag.data / fileInBag) should exist // N.B: without "original"
  }

  it should "use bag.data as upload root for and old dataset" in {
    val bag = newEmptyBag
      .addPayloadFile(randomContent, Paths.get("original/2.txt")).getOrRecover(payloadFailure)
    val fileInBag = "test.txt"

    DataFiles(save(bag), uuid)
      .write(randomContent, Paths.get(fileInBag)) shouldBe Success(true)

    (bag.data / "original" / fileInBag) should exist
  }

  it should "report write errors" in {
    val dataFiles = DataFiles(save(newEmptyBag), uuid)
    dataFiles.bag.data
      .createIfNotExists(asDirectory = true, createParents = true)
      .removePermission(PosixFilePermission.OWNER_WRITE)

    dataFiles.write("Lorem ipsum est".inputStream, Paths.get("location/in/data/dir/test.txt")).toString shouldBe
      Failure(new AccessDeniedException((dataFiles.bag.data / "original").toString())).toString
  }

  "delete" should "delete a file" in {
    val bag = newEmptyBag
    bag.addPayloadFile("Lorum ipsum est".inputStream, Paths.get("original/file.txt")).getOrRecover(payloadFailure)
    save(bag)
    (bag.data / "original" / "file.txt") should exist
    (bag.data.parent / "manifest-sha1.txt").lines.size shouldBe 1

    DataFiles(bag, uuid)
      .delete(Paths.get("file.txt")) should matchPattern { case Success(()) => }

    bag.data.children.size shouldBe 0
    (bag.data.parent / "manifest-sha1.txt").lines.size shouldBe 0
  }

  it should "recursively delete files" in {
    val bag = newEmptyBag
    bag.addPayloadFile("Lorum ipsum est".inputStream, Paths.get("original/file.txt")).getOrRecover(payloadFailure)
    (0 until 5).foreach { n =>
      bag.addPayloadFile(s"$n Lorum ipsum est".inputStream, Paths.get(s"original/path/to/file$n.txt")).getOrRecover(payloadFailure)
    }
    save(bag)
    (bag.data / "original").children.size shouldBe 2 // one file one folder
    (bag.baseDir / "manifest-sha1.txt").lines.size shouldBe 6

    DataFiles(bag, uuid)
      .delete(Paths.get("path/to")) should matchPattern { case Success(()) => }

    bag.data.children.size shouldBe 1
    (bag.baseDir / "manifest-sha1.txt").lines.size shouldBe 1
  }

  it should "report a non existing file" in {
    DataFiles(save(newEmptyBag), uuid)
      .delete(Paths.get("file.txt")) should
      matchPattern {
        case Failure(e: NoSuchFileInDepositException) if e.getMessage == "file.txt not found in deposit" =>
      }
  }

  "list" should "return files grouped by folder" in {
    val bag = newEmptyBag
      .addPayloadFile(randomContent, Paths.get("original/1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder1/b/x.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder1#b/x.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder1.x/y.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder1y/x.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder1/3.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/.hidden")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/#/1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder2/4.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/foo.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder11/4.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/folder1/5.txt")).getOrRecover(payloadFailure)

    DataFiles(save(bag), uuid).list(Paths.get(""))
      .map(_.map(fileInfo => fileInfo.dirpath.toString -> fileInfo.filename)) shouldBe Success(Seq(
      /* From path.compare:
       *
       * Compares two abstract paths lexicographically. The ordering defined by
       * this method is provider specific, and in the case of the default
       * provider, platform specific. This method does not access the file system
       * and neither file is required to exist.
       */
      /* Observation:
       *
       * depth first
       * at one level files an folders are mixed
       * a sequence of digits seems to be treated as a single character (unlike an ls command) but not numerically
       */
      "#" -> "1.txt",
      "" -> ".hidden",
      "" -> "1.txt",
      "folder1#b" -> "x.txt",
      "folder1.x" -> "y.txt",
      "folder1" -> "3.txt",
      "folder1" -> "5.txt",
      "folder1/b" -> "x.txt",
      "folder11" -> "4.txt",
      "folder1y" -> "x.txt",
      "folder2" -> "4.txt",
      "" -> "foo.txt",
    ))
  }

  it should "return an empty list on a new bag" in {
    DataFiles(save(newEmptyBag), uuid)
      .list() shouldBe Success(Seq.empty)
  }

  it should "return an empty list on original of a new bag" in {
    DataFiles(save(newEmptyBag), uuid)
      .list(Paths.get("original")) shouldBe Success(Seq.empty)
  }

  it should "return an empty list on foo/bar of a new bag" in {
    DataFiles(save(newEmptyBag), uuid)
      .list(Paths.get("foo/bar")) shouldBe Success(Seq.empty)
  }

  "fileInfo" should "contain proper information about the files" in {
    val sha1 = "a57ec0c3239f30b29f1e9270581be50a70c74c04"
    val sha2 = "815bc8056fe15e00f24514051f1d06016852360c"
    val expected = Seq(
      FileInfo("1.txt", Paths.get("some/"), sha1),
      FileInfo("2.txt", Paths.get("some/folder"), sha2),
    )
    val bag = newEmptyBag
      .addPayloadFile("lorum ipsum".inputStream, Paths.get("original/some/1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile("doler it".inputStream, Paths.get("original/some/folder/2.txt")).getOrRecover(payloadFailure)
    val dataFiles = DataFiles(save(bag), uuid)

    // get FileInfo for a singe file, alias GET /deposit/{id}/file/some/folder/2.txt
    dataFiles.get(Paths.get("some/folder/2.txt")) should matchPattern {
      case Success(FileInfo("2.txt", p, _)) if p.toString == "some/folder" =>
    }

    // get FileInfo for all files, alias GET /deposit/{id}/file
    inside(dataFiles.list()) { case Success(infos: Seq[_]) =>
      infos should contain theSameElementsAs expected
    }

    // get FileInfo recursively for a subfolder, alias GET /deposit/{id}/file/some
    inside(dataFiles.list(Paths.get("some"))) { case Success(infos: Seq[_]) =>
      infos should contain theSameElementsAs expected
    }
  }

  "moveAll" should "add payload files to the upload root of a new bag" in {
    (stagedDir / "sub" / "path").createDirectories()
    (stagedDir / "sub" / "path" / "some.thing").createFile().write("new content")
    (stagedDir / "some.thing").createFile().write("more content")
    val bag = save(newEmptyBag)
    bag.data.list shouldBe empty
    bag.fetchFiles shouldBe empty

    DataFiles(bag, uuid).moveAll(stagedDir, Paths.get("path/to")) shouldBe Success(())

    bag.fetchFiles shouldBe empty
    (bag.data / "original" / "path" / "to" / "some.thing").contentAsString shouldBe "more content"
    (bag.data / "original" / "path" / "to" / "sub" / "path" / "some.thing").contentAsString shouldBe "new content"
    stagedDir.listRecursively().filter(!_.isDirectory) shouldBe empty
  }

  it should "use bag.data as upload root for an old dataset" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = newEmptyBag.addPayloadFile(randomContent, Paths.get("1.txt")).getOrRecover(payloadFailure)

    DataFiles(save(bag), uuid).moveAll(stagedDir, Paths.get("")) shouldBe Success(())

    (bag.data / "some.thing") should exist
    bag.fetchFiles shouldBe empty
    stagedDir.listRecursively().filter(!_.isDirectory) shouldBe empty
  }

  it should "use bag.data as upload root when 'original' has siblings" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = newEmptyBag
      .addPayloadFile(randomContent, Paths.get("1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("original/2.txt")).getOrRecover(payloadFailure)

    DataFiles(save(bag), uuid).moveAll(stagedDir, Paths.get("")) shouldBe Success(())

    (bag.data / "some.thing") should exist
    bag.fetchFiles shouldBe empty
    stagedDir.listRecursively().filter(!_.isDirectory) shouldBe empty
  }

  it should "replace a fetch file" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val url = new URL("https://raw.githubusercontent.com/DANS-KNAW/easy-deposit-api/master/README.md")
    val bag = save(newEmptyBag.addFetchItem(
      url,
      Paths.get("original/path/to/some.thing"),
    ).getOrRecover { e =>
      assume(!e.isInstanceOf[UnknownHostException])
      fail(e)
    })
    bag.data.entries shouldBe empty
    bag.fetchFiles should not be empty

    DataFiles(bag, uuid).moveAll(stagedDir, Paths.get("path/to")) shouldBe a[Success[_]]

    bag.data / "original" / "path" / "to" / "some.thing" should exist
    bag.fetchFiles shouldBe empty
    stagedDir.listRecursively().filter(!_.isDirectory) shouldBe empty
  }

  it should "replace a payload file" in {
    (stagedDir / "some.thing").createFile().write("new content")
    val bag = save(newEmptyBag.addPayloadFile("Lorum ipsum".inputStream, Paths.get("original/path/to/some.thing")).getOrRecover(payloadFailure))
    val target = bag.data / "original" / "path" / "to" / "some.thing"
    target.contentAsString shouldBe "Lorum ipsum" // pre condition

    DataFiles(bag, uuid).moveAll(stagedDir, Paths.get("path/to")) shouldBe a[Success[_]]

    target.contentAsString shouldBe "new content" // post condition
    bag.fetchFiles shouldBe empty
    stagedDir.listRecursively().filter(!_.isDirectory) shouldBe empty
  }

  private def newEmptyBag = {
    DansV0Bag.empty(testDir / "draft").getOrRecover(e => fail("can't create test bag", e))
  }

  private def save(bag: DansV0Bag) = {
    bag.save.getOrRecover(e => fail("can't save test bag", e))
    bag
  }

  private def payloadFailure(value: Throwable) =
    fail("could not add payloadFile to test bag", value)

  private def randomContent =
    UUID.randomUUID().toString.inputStream
}
