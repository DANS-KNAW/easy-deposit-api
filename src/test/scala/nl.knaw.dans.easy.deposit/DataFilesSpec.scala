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

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
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

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val draftsDir = testDir / "drafts"

  "write" should "write content to the path specified" in {
    val dataFiles = createDatafiles
    val content = "Lorem ipsum est"
    val inputStream = new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))
    val fileInBag = "location/in/data/dir/test.txt"

    dataFiles.write(inputStream, Paths.get(fileInBag)) shouldBe Success(true)

    (dataFiles.bag.data / fileInBag).contentAsString shouldBe content
    inputStream.close()
  }

  it should "report write errors" in {
    val dataFiles = createDatafiles
    dataFiles.bag.data
      .createIfNotExists(asDirectory = true, createParents = true)
      .removePermission(PosixFilePermission.OWNER_WRITE)
    val inputStream = new ByteArrayInputStream("Lorem ipsum est".getBytes(StandardCharsets.UTF_8))

    dataFiles.write(inputStream, Paths.get("location/in/data/dir/test.txt")).toString shouldBe
      Failure(new AccessDeniedException((dataFiles.bag.data / "location").toString())).toString
    inputStream.close()
  }

  "delete" should "delete a file" in {
    val bag = DansV0Bag.empty(testDir / bagDirName).getOrRecover(e => fail(s"could not create test bag $e"))
    bag.addPayloadFile("Lorum ipsum est".inputStream, Paths.get("file.txt"))
    bag.save
    (bag.data / "file.txt") should exist
    (bag.data.parent / "manifest-sha1.txt").lines.size shouldBe 1

    DataFiles(bag)
      .delete(Paths.get("file.txt")) should matchPattern { case Success(()) => }

    bag.data.children.size shouldBe 0
    (bag.data.parent / "manifest-sha1.txt").lines.size shouldBe 0
  }

  it should "recursively delete files" in {
    val bag = DansV0Bag.empty(testDir / bagDirName).getOrRecover(e => fail(s"could not create test bag $e"))
    bag.addPayloadFile("Lorum ipsum est".inputStream, Paths.get("file.txt"))
    (0 until 5).foreach { n =>
      bag.addPayloadFile(s"$n Lorum ipsum est".inputStream, Paths.get(s"path/to/file$n.txt"))
    }
    bag.save
    bag.data.children.size shouldBe 2 // one file one folder
    (bag.baseDir / "manifest-sha1.txt").lines.size shouldBe 6

    DataFiles(bag)
      .delete(Paths.get("path/to")) should matchPattern { case Success(()) => }

    bag.data.children.size shouldBe 1
    (bag.baseDir / "manifest-sha1.txt").lines.size shouldBe 1
  }

  it should "report a non existing file" in {
    createDatafiles.delete(Paths.get("file.txt")) should matchPattern {
      case Failure(e: NoSuchFileInDepositException) if e.getMessage == "file.txt not found in deposit" =>
    }
  }

  "list" should "return files in lexicographical order" in {
    val bag = DansV0Bag
      .empty(testDir / "testBag").getOrRecover(fail("could not create test bag", _))
      .addPayloadFile(randomContent, Paths.get("1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("folder1/b/x.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("folder1#b/x.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("folder1/3.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("#/1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("folder2/4.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("folder11/4.txt")).getOrRecover(payloadFailure)
      .addPayloadFile(randomContent, Paths.get("folder1/5.txt")).getOrRecover(payloadFailure)
    bag.save()
    
    inside(DataFiles(bag).list(Paths.get(""))) {
      case Success(result) =>
        result.map(fileInfo => fileInfo.dirpath.toString -> fileInfo.filename) should contain inOrderOnly(
          "" -> "1.txt",
          "#" -> "1.txt",
          "folder1" -> "3.txt",
          "folder1" -> "5.txt",
          "folder1/b" -> "x.txt",
          "folder1#b" -> "x.txt",
          "folder11" -> "4.txt",
          "folder2" -> "4.txt",
        )
    }
  }

  "fileInfo" should "contain proper information about the files" in {
    val sha1 = "a57ec0c3239f30b29f1e9270581be50a70c74c04"
    val sha2 = "815bc8056fe15e00f24514051f1d06016852360c"
    val expected = Seq(
      FileInfo("1.txt", Paths.get("some/"), sha1),
      FileInfo("2.txt", Paths.get("some/folder"), sha2),
    )
    val bag = DansV0Bag
      .empty(testDir / "testBag").getOrRecover(fail("could not create test bag", _))
      .addPayloadFile("lorum ipsum".inputStream, Paths.get("some/1.txt")).getOrRecover(payloadFailure)
      .addPayloadFile("doler it".inputStream, Paths.get("some/folder/2.txt")).getOrRecover(payloadFailure)
    bag.save()
    val dataFiles = DataFiles(bag)

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

  private def createDatafiles = {
    DepositDir.create(draftsDir, "user01")
      .getOrRecover(e => fail("can't create test deposit", e))
      .getDataFiles.getOrRecover(e => fail("can't get datafiles from test deposit", e))
  }

  private def payloadFailure(value: Throwable) =
    fail("could not add payloadFile to test bag", value)

  private def randomContent =
    UUID.randomUUID().toString.inputStream
}
