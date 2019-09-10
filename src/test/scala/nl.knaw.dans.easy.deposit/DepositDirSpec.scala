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

import nl.knaw.dans.easy.deposit.Errors.CorruptDepositException
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import nl.knaw.dans.lib.error._
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success }

class DepositDirSpec extends TestSupportFixture with MockFactory {
  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    draftsDir.createDirectories()
    submitDir.createDirectories()
  }

  private val draftsDir = testDir / "drafts"
  private val submitDir = testDir / "submits"

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
        dir should exist
        (dir / "deposit.properties") should exist
        (dir / bagDirName) should exist
        (dir / bagDirName / "bag-info.txt") should exist
        (dir / bagDirName / "bagit.txt") should exist
        (dir / bagDirName / "manifest-sha1.txt") should exist
        (dir / bagDirName / "tagmanifest-sha1.txt") should exist
        (dir / bagDirName / "data") should exist
        (dir / bagDirName / "metadata") should exist

        val props = new PropertiesConfiguration() {
          load((dir / "deposit.properties").toJava)
        }
        props.getKeys.asScala.toSeq
          .map(key => key -> props.getString(key)).toArray should contain theSameElementsAs Array(
          "state.label" -> "DRAFT",
          "depositor.userId" -> "user001",
          "curation.performed" -> "no",
          "curation.required" -> "yes",
          "identifier.dans-doi.registered" -> "no",
          "creation.timestamp" -> "2018-03-22T20:43:01.000Z",
          "state.description" -> "Deposit is open for changes.",
          "identifier.dans-doi.action" -> "create",
          "bag-store.bag-name" -> bagDirName,
          "deposit.origin" -> "API",
        )
    }
  }

  "list" should """show no deposits of "user001" user""" in {
    DepositDir.list(draftsDir, "user001") shouldBe empty
  }

  it should """show one deposit of "user001" user""" in {
    DepositDir.create(draftsDir, "user001")
    DepositDir.list(draftsDir, "user001") should have length 1
  }

  it should """show more than two deposits of "user001" user""" in {
    for (_ <- 1 to 3) DepositDir.create(draftsDir, "user001")
    DepositDir.list(draftsDir, "user001") should have length 3
  }

  "get" should """return a specified deposit""" in {
    val deposit = createDepositAsPreparation("user001")
    val tryDeposit = DepositDir.get(draftsDir, "user001", deposit.id)
    tryDeposit shouldBe a[Success[_]]
    inside(tryDeposit) {
      case Success(dp) => dp shouldBe deposit
    }
  }

  "getDOI" should """return a new value""" in {
    // set up
    val user = "user001"
    val doi = "12345"
    val deposit = createDepositAsPreparation(user)
    val mdFile = deposit.draftBase / user / deposit.id.toString / bagDirName / "metadata" / "dataset.json"
    val depositPropertiesFile = deposit.draftBase / user / deposit.id.toString / "deposit.properties"
    val oldDepositProperties = depositPropertiesFile.lines.toSeq

    // preconditions
    mdFile.contentAsString shouldBe "{}"
    val pidMocker = mockPidRequester
    (pidMocker.requestPid(_: PidType)) expects * once() returning Success(doi)

    // test
    deposit.getDOI(pidMocker) shouldBe Success(doi)

    // post conditions
    mdFile.contentAsString should startWith(s"""{"identifiers":[{"scheme":"id-type:DOI","value":"12345"}]""")
    depositPropertiesFile.lines.toSeq
      .diff(oldDepositProperties) should contain only s"identifier.doi = $doi"
  }

  it should """complain about an invalid dataset""" in {
    val user = "user001"
    val doi = "12345"
    val deposit = createDepositAsPreparation(user)
    (deposit.draftBase / user / deposit.id.toString / bagDirName / "metadata" / "dataset.json").writeText(s"""{"doi":"$doi"}""")

    deposit.getDOI(null) should matchPattern { case Failure(CorruptDepositException(_, _, _)) => }
  }

  it should """return the available DOI""" in {
    val user = "user001"
    val doi = "12345"
    val deposit = createDepositAsPreparation(user)
    val dd = deposit.draftBase / user / deposit.id.toString
    (dd / bagDirName / "metadata" / "dataset.json").writeText(s"""{"identifiers":[{"scheme":"id-type:DOI","value":"12345"}]}""")
    (dd / "deposit.properties").writeText(s"identifier.doi = $doi")

    deposit.getDOI(null) shouldBe Success(doi)
  }

  private def createDepositAsPreparation(user: String): DepositDir = {
    DepositDir.create(draftsDir, user).getOrRecover(e => fail(e.toString, e))
  }
}
