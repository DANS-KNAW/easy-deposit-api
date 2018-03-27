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

import java.io.File
import java.nio.file.FileAlreadyExistsException

import scala.util.{ Failure, Success, Try }

class DepositDirSpec extends TestSupportFixture {
  private val draftsDir = testDir / "drafts"

  "createDirectory" should "throw FileAlreadyExistsException the directory \"drafts\" is read only" in {
    def getListErrorCheck() = {
      Try { draftsDir.createDirectory }
      match {
        case Failure(_) => throw new FileAlreadyExistsException("directory \"drafts\" is read only")
        case Success(_) => "success"
      }
    }
    if (draftsDir.exists()) {
      if (draftsDir.isReadable && !draftsDir.isWriteable) {
        a[FileAlreadyExistsException] should be thrownBy { getListErrorCheck() }
      }
    }
  }

  def createAndCheckDeposit(): Unit = {
    val dd = DepositDir.create(draftsDir, "user001")
    inside(dd) {
      case Success(d) =>
        val dir = draftsDir / "user001" / d.id.toString
        dir.toJava should exist
        (dir / "deposit.properties").toJava should exist
        (dir / "bag").toJava should exist
        (dir / "bag/bag-info.txt").toJava should exist
        (dir / "bag/bagit.txt").toJava should exist
        (dir / "bag/manifest-sha1.txt").toJava should exist
        (dir / "bag/tagmanifest-sha1.txt").toJava should exist
        (dir / "bag/data").toJava should exist
        (dir / "bag/metadata").toJava should exist
    }
  }

  "it" should "create a new directory with deposit.properties" in {
    createAndCheckDeposit()
  }

  "list" should "show no deposits of \"user001\" user" in {

    val userDir = new File(draftsDir + "/" + "user001")
    val listNoDeps = DepositDir.list(draftsDir, "user001")
    var numOfDeposits = 1
    listNoDeps match {
      case Success(_) => {
        numOfDeposits = listNoDeps.get.size
      }
      case Failure(_) => {
        if (!userDir.exists())
          numOfDeposits = 0
          println("user001 directory does not exist. Deposit directory is empty")
      }
    }
    println("Number of deposits = " + numOfDeposits)
    numOfDeposits should equal (0)
  }

  it should "show one deposit of \"user001\" user" in {

    createAndCheckDeposit()
    var numOfDeposits = 0
    val listOneDep = DepositDir.list(draftsDir, "user001")
    listOneDep match {
      case Success(_) => {
        numOfDeposits = listOneDep.get.size
      }
      case Failure(_) => "failure"
    }
    println("Number of deposits = " + numOfDeposits)
    numOfDeposits should equal (1)
  }

  it should "show more then two deposits of \"user001\" user" in {

    for (i <- 1 to 3) createAndCheckDeposit()
    var numOfDeposits = 0
    val listManyDeps = DepositDir.list(draftsDir, "user001")
    listManyDeps match {
      case Success(_) => {
        numOfDeposits = listManyDeps.get.size
      }
      case Failure(_) => "failure"
    }
    println("Number of deposits = " + numOfDeposits)
    numOfDeposits should be > 2
  }
}
