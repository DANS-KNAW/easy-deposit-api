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

    val (depositDir, mdDir, propsFile) = createDeposit(datasetMetadata.copy(messageForDataManager = Some(customMessage)))
    propsFile.append(s"identifier.doi=$doi")
    (mdDir.parent / "data" / "text.txt").touch()
    (mdDir.parent / "data" / "folder").createDirectories()
    (mdDir.parent / "data" / "folder" / "text.txt").write("Lorum ipsum")
    val mdOldSize = (mdDir / "dataset.json").size
    depositDir.getStateInfo should matchPattern {
      case Success(StateInfo(State.draft, "Deposit is open for changes.")) =>
    }

    new Submitter(testDir / "staged", null, null).submit(depositDir) should matchPattern {
      case Failure(e) if e.isInstanceOf[NotImplementedError] =>
    }

    val prologue = """<?xml version='1.0' encoding='UTF-8'?>"""
    (mdDir / "dataset.json").size shouldBe mdOldSize
    (mdDir / "message-from-depositor.txt").contentAsString shouldBe customMessage
    (mdDir / "agreements.xml").lineIterator.next() shouldBe prologue
    (mdDir / "dataset.xml").lineIterator.next() shouldBe prologue
    (mdDir / "files.xml").contentAsString should include("""filepath="data/text.txt""")
    (mdDir / "files.xml").contentAsString should include("""filepath="data/folder/text.txt""")
    depositDir.getDOI(null) shouldBe Success(doi)
    depositDir.getStateInfo should matchPattern {
      case Success(StateInfo(State.submitted, "Deposit is ready for processing.")) =>
    }
  }

  it should "write empty message-from-depositor file" in {

    val (depositDir, mdDir, propsFile) = createDeposit(datasetMetadata.copy(messageForDataManager = None))
    propsFile.append(s"identifier.doi=$doi")

    new Submitter(testDir / "staged", null, null).submit(depositDir) should matchPattern {
      case Failure(e) if e.isInstanceOf[NotImplementedError] =>
    }

    (mdDir / "message-from-depositor.txt").contentAsString shouldBe ""
  }

  it should "add DOI to props and json" in {

    val (depositDir, _, _) = createDeposit(datasetMetadata.copy(identifiers = None))
    val pidMocker = mock[PidRequester]
    val mockedPid = "12345"
    (pidMocker.requestPid(_: PidType)) expects * once() returning Success(mockedPid)

    new Submitter(testDir / "staged", null, pidMocker).submit(depositDir) should matchPattern {
      case Failure(e) if e.isInstanceOf[NotImplementedError] =>
    }

    depositDir.getDOI(null) shouldBe Success(mockedPid)
  }

  it should "reject an inconsistent DOI" in {
    // invalid state transition is tested with IntegrationSpec

    val (depositDir, _, _) = createDeposit(datasetMetadata)

    new Submitter(testDir / "staged", null, null).submit(depositDir) should matchPattern {
      case Failure(e) if e.isInstanceOf[CorruptDepositException] =>
    }
  }

  it should "reject an incomplete json" in {
    // other validation errors are tested with DatasetXmlSpec and DepositDirSpec

    val (depositDir, _, _) = createDeposit(DatasetMetadata())
    val pidMocker = mock[PidRequester]
    val mockedPid = "12345"
    (pidMocker.requestPid(_: PidType)) expects * once() returning Success(mockedPid)

    new Submitter(testDir / "staged", null, pidMocker).submit(depositDir) should matchPattern {
      case Failure(e) if e.isInstanceOf[InvalidDocumentException] =>
    }
  }

  private def getMetadataDir(depositDir: DepositDir) = {
    depositDir
      .getDataFiles
      .getOrRecover(e => fail(e.toString, e))
      .filesMetaData
      .parent
  }

  private def createDeposit(metadata: DatasetMetadata) = {
    val depositDir = DepositDir.create(testDir / "drafts", "user").getOrRecover(e => fail(e.toString, e))
    depositDir.writeDatasetMetadataJson(metadata)
    val mdDir = getMetadataDir(depositDir)
    (depositDir, mdDir, mdDir.parent.parent / "deposit.properties")
  }
}
