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

import java.nio.file.attribute.PosixFilePermission

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success }

class DepositDirSpec extends TestSupportFixture with MockFactory {
  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    draftsDir.createDirectories()
  }

  private val draftsDir = testDir / "drafts"

  "DepositDir.create" should "fail if the dir 'draft' is read only" in {
    draftsDir
      .removePermission(PosixFilePermission.OWNER_WRITE)
    inside(DepositDir.create(draftsDir, "user001")) {
      case Failure(_) =>
    }
  }

  it should "create a new directory with deposit.properties" in {
    val tryDeposit = DepositDir.create(draftsDir, "user001")
    tryDeposit shouldBe a[Success[_]]
    inside(tryDeposit) {
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

  "list" should """show no deposits of "user001" user""" in {
    val tryDeposits = DepositDir.list(draftsDir, "user001")
    tryDeposits shouldBe a[Success[_]]
    inside(tryDeposits) {
      case Success(list) => list shouldBe empty
    }
  }

  it should """show one deposit of "user001" user""" in {
    DepositDir.create(draftsDir, "user001")
    val tryDeposits = DepositDir.list(draftsDir, "user001")
    tryDeposits shouldBe a[Success[_]]
    inside(tryDeposits) {
      case Success(list) => list should have length 1
    }
  }

  it should """show more than two deposits of "user001" user""" in {
    for (i <- 1 to 3) DepositDir.create(draftsDir, "user001")
    val tryDeposits = DepositDir.list(draftsDir, "user001")
    tryDeposits shouldBe a[Success[_]]
    inside(tryDeposits) {
      case Success(list) => list should have length 3
    }
  }

  "get" should """return a specified deposit""" in {
    val deposit = DepositDir.create(draftsDir, "user001").get
    val tryDeposit = DepositDir.get(draftsDir, "user001", deposit.id)
    tryDeposit shouldBe a[Success[_]]
    inside(tryDeposit) {
      case Success(dp) => dp shouldBe deposit
    }
  }

  "getDOI" should """return a new value""" in {
    val user = "user001"
    val doi = "12345"
    val deposit = DepositDir.create(draftsDir, user).get
    val mdFile = deposit.baseDir / user / deposit.id.toString / "bag" / "metadata" / "dataset.json"
    mdFile.contentAsString shouldBe "{}"

    val pidMocker = mock[PidRequester]
    (pidMocker.requestPid(_: PidType)) expects * once() returning Success(doi)

    deposit.getDOI(pidMocker) shouldBe Success(doi)
    mdFile.contentAsString should startWith(s"""{"doi":"$doi",""")
  }

  it should """complain about an invalid dataset""" in {
    val user = "user001"
    val doi = "12345"
    val deposit = DepositDir.create(draftsDir, user).get
    (deposit.baseDir / user / deposit.id.toString / "bag" / "metadata" / "dataset.json").writeText("""{"doi":"$doi"}""")

    val pidMocker = mock[PidRequester] // note that no pid is requested

    deposit.getDOI(pidMocker) shouldBe a[Failure[_]]
  }

  it should """return the available DOI""" in {
    val user = "user001"
    val doi = "12345"
    val deposit = DepositDir.create(draftsDir, user).get
    val dd = deposit.baseDir / user / deposit.id.toString
    (dd / "bag" / "metadata" / "dataset.json").writeText(s"""{"doi":"$doi"}""")
    (dd / "deposit.properties").writeText(s"""identifier.doi = $doi""")

    val pidMocker = mock[PidRequester] // note that no pid is requested

    deposit.getDOI(pidMocker) shouldBe Success(doi)
  }
}
