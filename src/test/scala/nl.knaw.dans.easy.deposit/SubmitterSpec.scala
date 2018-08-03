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

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, StateInfo }
import nl.knaw.dans.lib.error._
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success, Try }

class SubmitterSpec extends TestSupportFixture with MockFactory {
  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val customMessage = "Lorum ipsum"
  private val datasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
    .getOrRecover(e => fail("could not get test input", e))
  private val doi = Try { datasetMetadata.identifiers.get.headOption.get.value }
    .getOrRecover(e => fail("could not get DOI from test input", e))

  "submit" should "write 4 files" in {

    // preparations
    val depositDir = createDeposit(datasetMetadata.copy(messageForDataManager = Some(customMessage)))
    val bagDir = getBagDir(depositDir)
    (bagDir.parent / "deposit.properties").append(s"identifier.doi=$doi")
    (bagDir / "data" / "text.txt").touch()
    (bagDir / "data" / "folder").createDirectories()
    (bagDir / "data" / "folder" / "text.txt").write("Lorum ipsum")
    (testDir / "submitted").createDirectories()

    // preconditions
    val mdOldSize = (bagDir / "metadata" / "dataset.json").size // should not change
    depositDir.getStateInfo should matchPattern { // should change
      case Success(StateInfo(State.draft, "Deposit is open for changes.")) =>
    }

    // the test
    new Submitter(testDir / "staged", testDir / "submitted", null)
      .submit(depositDir) should matchPattern { case Success(()) => }

    // post conditions
    (testDir / "staged").children.size shouldBe 0
    (bagDir / "metadata" / "dataset.json").size shouldBe mdOldSize // no DOI added
    val submittedBagDir = testDir / "submitted" / depositDir.id.toString / "bag"
    (submittedBagDir / "metadata" / "message-from-depositor.txt").contentAsString shouldBe customMessage
    (submittedBagDir / "metadata" / "agreements.xml").lineIterator.next() shouldBe prologue
    (submittedBagDir / "metadata" / "dataset.xml").lineIterator.next() shouldBe prologue
    (submittedBagDir / "data").children.size shouldBe (bagDir / "data").children.size
    (submittedBagDir / "tagmanifest-sha1.txt").lines.size shouldBe 7 // tag files including metadata/*
    (submittedBagDir / "manifest-sha1.txt").lines.size shouldBe 2 // the data files
    (submittedBagDir / "metadata" / "files.xml").contentAsString.matches("(?s).*(filepath=.*){2}.*") shouldBe true
    (submittedBagDir.parent / "deposit.properties").contentAsString shouldBe
      (bagDir.parent / "deposit.properties").contentAsString
    depositDir.getDOI(null) shouldBe Success(doi) // no pid-requester so obtained from json and/or props
    depositDir.getStateInfo should matchPattern {
      case Success(StateInfo(State.submitted, "Deposit is ready for processing.")) =>
    }
  }

  it should "write empty message-from-depositor file" in {

    val depositDir = createDeposit(datasetMetadata.copy(messageForDataManager = None))
    val bagDir = getBagDir(depositDir)
    (bagDir.parent / "deposit.properties").append(s"identifier.doi=$doi")
    (testDir / "submitted").createDirectories()

    new Submitter(testDir / "staged", testDir / "submitted", null)
      .submit(depositDir) should matchPattern { case Success(()) => }

    (testDir / "submitted" / depositDir.id.toString / "bag" / "metadata" / "message-from-depositor.txt")
      .contentAsString shouldBe ""
  }

  it should "add DOI to props and json" in {

    val depositDir = createDeposit(datasetMetadata.copy(identifiers = None))
    val pidMocker = mock[PidRequester]
    val mockedPid = "12345"
    (pidMocker.requestPid(_: PidType)) expects * once() returning Success(mockedPid)
    (testDir / "submitted").createDirectories()

    new Submitter(testDir / "staged", testDir / "submitted", pidMocker)
      .submit(depositDir) should matchPattern { case Success(()) => }

    depositDir.getDOI(null) shouldBe Success(mockedPid)
  }

  it should "reject an inconsistent DOI" in {
    // invalid state transition is tested with IntegrationSpec

    val depositDir = createDeposit(datasetMetadata)

    new Submitter(testDir / "staged", testDir / "submitted", null).submit(depositDir) should matchPattern {
      case Failure(e) if e.isInstanceOf[CorruptDepositException] =>
    }
  }

  it should "reject an incomplete json" in {
    // other validation errors are tested with DatasetXmlSpec and DepositDirSpec

    val depositDir = createDeposit(DatasetMetadata())
    val pidMocker = mock[PidRequester]
    val mockedPid = "12345"
    (pidMocker.requestPid(_: PidType)) expects * once() returning Success(mockedPid)

    new Submitter(testDir / "staged", testDir / "submitted", pidMocker).submit(depositDir) should matchPattern {
      case Failure(e) if e.isInstanceOf[InvalidDocumentException] =>
    }
  }

  private def getBagDir(depositDir: DepositDir) = {
    depositDir
      .getDataFiles
      .getOrRecover(e => fail(e.toString, e))
      .dataFilesBase
      .parent
  }

  private def createDeposit(metadata: DatasetMetadata) = {
    val depositDir = DepositDir.create(testDir / "drafts", "user").getOrRecover(e => fail(e.toString, e))
    depositDir.writeDatasetMetadataJson(metadata)
    depositDir
  }
}
