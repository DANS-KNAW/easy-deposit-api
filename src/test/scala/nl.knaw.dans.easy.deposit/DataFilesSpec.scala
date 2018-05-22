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

import scala.util.{ Failure, Success }

class DataFilesSpec extends TestSupportFixture {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val draftsDir = testDir / "drafts"
  private val dataFiles = DepositDir(draftsDir, "user01", uuid)
    .getDataFiles
    .getOrElse(throw new Exception("test preparations failed"))

  "write" should "write content to the path specified" in {
    val content = "Lorem ipsum est"
    val inputStream = new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))
    val fileInBag = "location/in/data/dir/test.txt"

    dataFiles.write(inputStream, Paths.get(fileInBag)) shouldBe Success(true)

    (dataFiles.dataFilesBase / fileInBag).contentAsString shouldBe content
    inputStream.close()
  }

  it should "report write errors" in {
    dataFiles.dataFilesBase
      .createIfNotExists(asDirectory = true, createParents = true)
      .removePermission(PosixFilePermission.OWNER_WRITE)
    val inputStream = new ByteArrayInputStream("Lorem ipsum est".getBytes(StandardCharsets.UTF_8))

    dataFiles.write(inputStream, Paths.get("location/in/data/dir/test.txt")).toString shouldBe
      Failure(new AccessDeniedException((dataFiles.dataFilesBase / "location").toString())).toString
    inputStream.close()
  }

  "delete" should "delete a file" in {
    val dataFilesBase = dataFiles.dataFilesBase
      .createIfNotExists(asDirectory = true, createParents = true)
    val file = (dataFilesBase / "file.txt").writeText("Lorem ipsum est")

    dataFiles.delete(Paths.get("file.txt")) should matchPattern { case Success(()) => }

    file.toJava shouldNot exist
  }


  it should "recursively delete files" in {
    val dir = (dataFiles.dataFilesBase / "path" / "to").createIfNotExists(asDirectory = true, createParents = true)
    (dir / "file1.txt").writeText("Lorem ipsum est")
    (dir / "file2.txt").writeText("Lorem ipsum dolor sit amet")

    dataFiles.delete(Paths.get("path/to")) should matchPattern { case Success(()) => }

    (dataFiles.dataFilesBase / "path" / "to").toJava shouldNot exist
    (dataFiles.dataFilesBase / "path").toJava should exist
  }

  it should "report a non existing file" in {
    val path = (dataFiles.dataFilesBase / "file.txt").toString()
    inside(dataFiles.delete(Paths.get("file.txt"))) {
      case Failure(e: NoSuchFileException) => e.getMessage shouldBe path
      case other => fail(s"expecting Failure(NoSuchFileException($path)) but got $other")
    }
  }
}
