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
import java.nio.file.{ AccessDeniedException, NoSuchFileException, Paths }
import java.util.UUID
import java.util.zip.ZipInputStream

import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.BadRequestException
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


  "unzip" should "report invalid content" in pendingUntilFixed {
    val dataFiles = createDatafiles
    val content = "Lorem ipsum est"
    val fileInBag = "location/in/data/dir/test.txt"
    val zipInputStream = new ZipInputStream(new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8)))

    dataFiles.unzip(zipInputStream, Paths.get(fileInBag)) should matchPattern {
      case Failure(BadRequestException(s)) if s == "" =>
    }
    zipInputStream.close()
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
    val bag = DansV0Bag.empty(testDir / "bag").getOrRecover(e => fail(s"could not create test bag $e"))
    bag.addPayloadFile("Lorum ipsum est".asInputStream, Paths.get("file.txt"))
    bag.save
    (bag.data / "file.txt").toJava should exist
    (bag.data.parent / "manifest-sha1.txt").lines.size shouldBe 1

    DataFiles(bag)
      .delete(Paths.get("file.txt")) should matchPattern { case Success(()) => }

    bag.data.children.size shouldBe 0
    (bag.data.parent / "manifest-sha1.txt").lines.size shouldBe 0
  }

  it should "recursively delete files" in {
    val bag = DansV0Bag.empty(testDir / "bag").getOrRecover(e => fail(s"could not create test bag $e"))
    bag.addPayloadFile("Lorum ipsum est".asInputStream, Paths.get("file.txt"))
    (0 until 5).foreach { n =>
      bag.addPayloadFile(s"$n Lorum ipsum est".asInputStream, Paths.get(s"path/to/file$n.txt"))
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
    inside(createDatafiles.delete(Paths.get("file.txt"))) {
      case Failure(e: NoSuchFileException) => e.getMessage shouldBe "file.txt"
      case other => fail(s"expecting Failure(NoSuchFileException(file.txt)) but got $other")
    }
  }

  "list" should "return the proper number of files" in {
    val bag = DansV0Bag
      .empty(testDir / "testBag").getOrRecover(fail("could not create test bag", _))
      .addPayloadFile(randomContent)(_ / "1.txt").getOrRecover(payloadFailure)
      .addPayloadFile(randomContent)(_ / "folder1/2.txt").getOrRecover(payloadFailure)
      .addPayloadFile(randomContent)(_ / "folder1/3.txt").getOrRecover(payloadFailure)
      .addPayloadFile(randomContent)(_ / "folder2/4.txt").getOrRecover(payloadFailure)
    bag.save()
    val dataFiles = DataFiles(bag)
    val path2 = Paths.get("folder2")

    dataFiles.list(path2) should matchPattern {
      case Success(Seq(FileInfo("4.txt", p, _))) if p == path2 => // no order problem as the next cases would have
    }
    dataFiles.list(Paths.get("folder1")) should matchPattern {
      case Success(Seq(_, _)) =>
    }
    dataFiles.list(Paths.get("")) should matchPattern {
      case Success(Seq(_, _, _, _)) =>
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
    UUID.randomUUID().toString.asInputStream
}
