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

import java.nio.file.Paths

import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, StateInfo }
import nl.knaw.dans.lib.error._
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success }

class SubmitterSpec extends TestSupportFixture with MockFactory {
  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  private val customMessage = "Lorum ipsum"
  private val datasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
    .getOrRecover(e => fail("could not get test input", e))
  private val doi = datasetMetadata.doi
    .getOrElse(fail("could not get DOI from test input"))

  "submit" should "write all files" in {

    // preparations
    val depositDir = createDeposit(datasetMetadata.copy(messageForDataManager = Some(customMessage)))
    val bag = getBag(depositDir)
    val bagDir = bag.baseDir
    (bagDir.parent / "deposit.properties").append(s"identifier.doi=$doi")
    bag.addPayloadFile("".asInputStream, Paths.get("text.txt"))
    bag.addPayloadFile("Lorum ipsum".asInputStream, Paths.get("folder/text.txt"))
    bag.save()
    (testDir / "submitted").createDirectories()

    // preconditions
    val mdOldSize = (bagDir / "metadata" / "dataset.json").size // should not change
    depositDir.getStateInfo should matchPattern { // should change
      case Success(StateInfo(State.draft, "Deposit is open for changes.")) =>
    }

    assumeSchemaAvailable
    // the test
    new Submitter(testDir / "staged", testDir / "submitted")
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

    assumeSchemaAvailable
    new Submitter(testDir / "staged", testDir / "submitted")
      .submit(depositDir) should matchPattern { case Success(()) => }

    (testDir / "submitted" / depositDir.id.toString / "bag" / "metadata" / "message-from-depositor.txt")
      .contentAsString shouldBe ""
  }

  it should "report a file missing in the draft" in {
    val depositDir = createDeposit(datasetMetadata)
    val bag = getBag(depositDir)
    (bag.baseDir.parent / "deposit.properties").append(s"identifier.doi=$doi")
    bag.addPayloadFile("lorum ipsum".asInputStream, Paths.get("file.txt"))
    bag.save()
    (testDir / "submitted").createDirectories()

    // add file to manifest that does not exist
    (bag.baseDir / "manifest-sha1.txt").append("chk file")

    assumeSchemaAvailable
    new Submitter(testDir / "staged", testDir / "submitted").submit(depositDir) should matchPattern {
      case Failure(e) if e.getMessage == s"invalid bag, missing [files, checksums]: [Set($testDir/drafts/user/${ depositDir.id }/bag/file), Set()]" =>
    }
  }

  it should "report an invalid checksum" in {
    val depositDir = createDeposit(datasetMetadata)
    val bag = getBag(depositDir)
    (bag.baseDir.parent / "deposit.properties").append(s"identifier.doi=$doi")
    bag.addPayloadFile("lorum ipsum".asInputStream, Paths.get("file.txt"))
    bag.save()
    (testDir / "submitted").createDirectories()

    // change a checksum in the manifest
    val manifest = bag.baseDir / "manifest-sha1.txt"
    manifest.write(manifest.contentAsString.replaceAll(" +", "xxx  "))

    val checksum = "a57ec0c3239f30b29f1e9270581be50a70c74c04"
    assumeSchemaAvailable
    new Submitter(testDir / "staged", testDir / "submitted").submit(depositDir) should matchPattern {
      case Failure(e)
        if e.getMessage == s"staged and draft bag [${ bag.baseDir.parent }] have different payload manifest elements: (Set((data/file.txt,$checksum)),Set((data/file.txt,${ checksum }xxx)))" =>
    }
  }

  private def getBagDir(depositDir: DepositDir) = getBag(depositDir).baseDir

  private def getBag(depositDir: DepositDir) = {
    depositDir
      .getDataFiles
      .getOrRecover(e => fail(e.toString, e))
      .bag
  }

  private def createDeposit(metadata: DatasetMetadata) = {
    val depositDir = DepositDir.create(testDir / "drafts", "user").getOrRecover(e => fail(e.toString, e))
    depositDir.writeDatasetMetadataJson(metadata)
    depositDir
  }
}
