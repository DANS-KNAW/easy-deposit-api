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

import java.net.URL
import java.nio.file.Paths
import java.util.concurrent.{ LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit }

import better.files.StringExtensions
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.easy.deposit.docs._
import nl.knaw.dans.easy.deposit.executor.JobQueueManager
import nl.knaw.dans.lib.error._
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success }

class SubmitJobSpec extends TestSupportFixture with MockFactory {
  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    (testDir / "submitted").createDirectories()
  }

  private val easyHome: URL = new URL("https://easy.dans.knaw.nl/ui")
  private val datasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
    .getOrRecover(e => fail("could not get test input", e))
  private val doi = datasetMetadata.doi
    .getOrElse(fail("could not get DOI from test input"))

  "submit" should "write empty message-from-depositor file" ignore {
    val depositDir = createDeposit(datasetMetadata.copy(messageForDataManager = None))
    addDoiToDepositProperties(getBag(depositDir))

    val bagStoreBagId = succeedingSubmit(depositDir)

    (testDir / "submitted" / bagStoreBagId / bagDirName / "metadata" / "depositor-info" / "message-from-depositor.txt")
      .contentAsString shouldBe s"The deposit can be found at http://does.not.exist/${ depositDir.id }"
  }

  it should "overwrite a previous bag-store.bag-id at resubmit" ignore {
    val depositDir = createDeposit(datasetMetadata.copy(messageForDataManager = None))
    addDoiToDepositProperties(getBag(depositDir))

    // submit twice (skipping ingest-flow or a curator changed the state to rejected)
    val bagStoreBagId1 = succeedingSubmit(depositDir)
    val depositProps = depositDir.bagDir.parent / "deposit.properties"
    depositProps.write(depositProps.contentAsString.replace("SUBMITTED", "DRAFT"))
    val bagStoreBagId2 = succeedingSubmit(depositDir)

    // we have both submits
    bagStoreBagId1 should not be depositDir.id.toString
    bagStoreBagId2 should not be depositDir.id.toString
    bagStoreBagId1 should not be bagStoreBagId2
    (testDir / "submitted" / bagStoreBagId1) should exist
    (testDir / "submitted" / bagStoreBagId2) should exist

    // we only have the second bag-store.bag-id in the properties
    bagStoreBagId1 should not be bagStoreBagId2
    val lines = depositProps.contentAsString.split("\n")
    lines.count(_.contains("bag-store.bag-id")) shouldBe 1
    lines.count(_ == s"bag-store.bag-id = $bagStoreBagId2") shouldBe 1
  }

  private def succeedingSubmit(deposit: DepositDir): String = {
    val stateManager = deposit.getStateManager(testDir / "submitted", easyHome)
      .getOrRecover(e => fail(s"could not get stateManager of test deposit $e"))

    val triedBagStoreBagID = createSubmitter(userGroup).submit(deposit, stateManager, defaultUserInfo, testDir / "staged")
    triedBagStoreBagID shouldBe a[Success[_]]
    triedBagStoreBagID
      .map(_.toString)
      .getOrElse(throw new Exception("should not get here"))
  }

  it should "report a file missing in the draft" ignore {
    val depositDir = createDeposit(datasetMetadata)
    val stateManager = depositDir.getStateManager(testDir / "submitted", easyHome)
      .getOrRecover(e => fail(s"could not get stateManager of test deposit $e"))
    val bag = getBag(depositDir)
    addDoiToDepositProperties(bag)
    bag.addPayloadFile("lorum ipsum".inputStream, Paths.get("file.txt"))
    bag.save()

    // add file to manifest that does not exist
    (bag.baseDir / "manifest-sha1.txt").append("chk file")

    createSubmitter(userGroup).submit(depositDir, stateManager, defaultUserInfo, testDir / "staged") should matchPattern {
      case Failure(e) if e.getMessage == s"invalid bag, missing [files, checksums]: [Set($testDir/drafts/user/${ depositDir.id }/bag/file), Set()]" =>
    }
  }

  it should "report an invalid checksum" ignore {
    val depositDir = createDeposit(datasetMetadata)
    val stateManager = depositDir.getStateManager(testDir / "submitted", easyHome)
      .getOrRecover(e => fail(s"could not get stateManager of test deposit $e"))
    val bag = getBag(depositDir)
    addDoiToDepositProperties(bag)
    bag.addPayloadFile("lorum ipsum".inputStream, Paths.get("file.txt"))
    bag.save()

    // change a checksum in the manifest
    val manifest = bag.baseDir / "manifest-sha1.txt"
    manifest.write(manifest.contentAsString.replaceAll(" +", "xxx  "))

    val checksum = "a57ec0c3239f30b29f1e9270581be50a70c74c04"
    createSubmitter(userGroup).submit(depositDir, stateManager, defaultUserInfo, testDir / "staged") should matchPattern {
      case Failure(e)
        if e.getMessage == s"staged and draft bag [${ bag.baseDir.parent }] have different payload manifest elements: (Set((data/file.txt,$checksum)),Set((data/file.txt,${ checksum }xxx)))" =>
    }
  }

  private def createSubmitter(group: String): Submitter = {
    createSubmitterWithStubs((testDir / "submitted").createDirectories(), group, "http://does.not.exist", new JobQueueManager(new ThreadPoolExecutor(1, 1, 10, TimeUnit.SECONDS, new LinkedBlockingQueue())))
  }

  private def addDoiToDepositProperties(bag: DansBag): Unit = {
    (bag.baseDir.parent / "deposit.properties").append(s"identifier.doi=$doi")
  }

  private def getBag(depositDir: DepositDir): DansBag = {
    depositDir
      .getDataFiles
      .getOrRecover(e => fail(e.toString, e))
      .bag
  }

  private def createDeposit(metadata: DatasetMetadata): DepositDir = {
    val depositDir = DepositDir.create(testDir / "drafts", "user").getOrRecover(e => fail(e.toString, e))
    depositDir.writeDatasetMetadataJson(metadata)
    depositDir
  }
}
