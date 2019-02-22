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

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import nl.knaw.dans.easy.deposit.docs.StateInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
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
        dir should exist
        (dir / "deposit.properties") should exist
        (dir / "bag") should exist
        (dir / "bag/bag-info.txt") should exist
        (dir / "bag/bagit.txt") should exist
        (dir / "bag/manifest-sha1.txt") should exist
        (dir / "bag/tagmanifest-sha1.txt") should exist
        (dir / "bag/data") should exist
        (dir / "bag/metadata") should exist

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
          "bag-store.bag-id" -> d.id.toString,
          "state.description" -> "Deposit is open for changes.",
          "identifier.dans-doi.action" -> "create",
        )
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
    for (_ <- 1 to 3) DepositDir.create(draftsDir, "user001")
    val tryDeposits = DepositDir.list(draftsDir, "user001")
    tryDeposits shouldBe a[Success[_]]
    inside(tryDeposits) {
      case Success(list) => list should have length 3
    }
  }

  "get" should """return a specified deposit""" in {
    val deposit = createDepositAsPreparation("user001")
    val tryDeposit = DepositDir.get(draftsDir, "user001", deposit.id)
    tryDeposit shouldBe a[Success[_]]
    inside(tryDeposit) {
      case Success(dp) => dp shouldBe deposit
    }
  }

  "setStateInfo" should "result in Success when transitioning from DRAFT to SUBMITTED" in {
    val deposit = createDepositAsPreparation("user001")
    val propsFile = draftsDir / "user001" / deposit.id.toString / "deposit.properties"
    deposit.setStateInfo(StateInfo(State.submitted, "Ready for processing")) shouldBe a[Success[_]]
    propsFile.contentAsString should include regex """state.label\s*=\s*SUBMITTED""".r
  }

  it should "result in Success when transitioning from REJECTED to DRAFT" in {
    val deposit = createDepositAsPreparation("user001")
    val propsFile = draftsDir / "user001" / deposit.id.toString / "deposit.properties"
    val old = propsFile.contentAsString
    propsFile.write(old.replaceFirst("DRAFT", "REJECTED"))
    deposit.setStateInfo(StateInfo(State.draft, "Open for changes")) shouldBe a[Success[_]]
    propsFile.contentAsString should include regex """state.label\s*=\s*DRAFT""".r
  }

  it should "result in IllegalStateTransitionException when transitioning from DRAFT to ARCHIVED" in {
    val deposit = createDepositAsPreparation("user001")
    deposit.setStateInfo(StateInfo(State.archived, "Completed archival process")) should matchPattern {
      case Failure(IllegalStateTransitionException("user001", _, State.draft, State.archived)) =>
    }
  }

  it should "result in IllegalStateTransitionException when transitioning from REJECTED to ARCHIVED" in {
    val deposit = DepositDir.create(draftsDir, "user001").get
    val propsFile = draftsDir / "user001" / deposit.id.toString / "deposit.properties"
    val old = propsFile.contentAsString
    propsFile.write(old.replaceFirst("DRAFT", "REJECTED"))
    deposit.setStateInfo(StateInfo(State.archived, "Completed archival process")) should matchPattern {
      case Failure(IllegalStateTransitionException("user001", _, State.`rejected`, State.archived)) =>
    }
  }

  "getDOI" should """return a new value""" in {
    // set up
    val user = "user001"
    val doi = "12345"
    val deposit = createDepositAsPreparation(user)
    val mdFile = deposit.baseDir / user / deposit.id.toString / "bag" / "metadata" / "dataset.json"
    val depositPropertiesFile = deposit.baseDir / user / deposit.id.toString / "deposit.properties"
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
    (deposit.baseDir / user / deposit.id.toString / "bag" / "metadata" / "dataset.json").writeText(s"""{"doi":"$doi"}""")

    deposit.getDOI(null) should matchPattern { case Failure(CorruptDepositException(_, _, _)) => }
  }

  it should """return the available DOI""" in {
    val user = "user001"
    val doi = "12345"
    val deposit = createDepositAsPreparation(user)
    val dd = deposit.baseDir / user / deposit.id.toString
    (dd / "bag" / "metadata" / "dataset.json").writeText(s"""{"identifiers":[{"scheme":"id-type:DOI","value":"12345"}]}""")
    (dd / "deposit.properties").writeText(s"identifier.doi = $doi")

    deposit.getDOI(null) shouldBe Success(doi)
  }

  private def createDepositAsPreparation(user: String) = {
    DepositDir.create(draftsDir, user).getOrRecover(e => fail(e.toString, e))
  }
}
