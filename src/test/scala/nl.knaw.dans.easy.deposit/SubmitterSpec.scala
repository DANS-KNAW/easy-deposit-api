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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata
import org.joda.time.DateTime
import nl.knaw.dans.lib.error._

import scala.util.{ Failure, Success }

class SubmitterSpec extends TestSupportFixture {
  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    //draftsDir.createDirectories()
  }

  private val draftsDir = testDir / "drafts"

  "submit" should "write 4 files" in {
    val prologue = """<?xml version='1.0' encoding='UTF-8'?>"""
    val message = "Lorum ipsum"
    val datasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
      .getOrRecover(e => fail(e.toString))
      .copy(messageForDataManager = Some(message))
    val depositDir = DepositDir.create(draftsDir, "user").getOrRecover(e => fail(e.toString))
    val mdDir = depositDir.getDataFiles.getOrRecover(e => fail(e.toString))
      .filesMetaData.parent.createIfNotExists(asDirectory = true, createParents = true)
    depositDir.writeDatasetMetadataJson(datasetMetadata)
    val oldSize = (mdDir / "dataset.json").size
    (mdDir.parent / "data" / "text.txt").touch()

    new Submitter(null,null).submit(depositDir) shouldBe a[Failure[_]] // implementation incomplete

    (mdDir / "dataset.json").size shouldBe oldSize // the dataset.json file is not changed
    (mdDir / "message-from-depositor.txt").contentAsString shouldBe message
    (mdDir / "agreements.xml").lineIterator.next() shouldBe prologue
    (mdDir / "dataset.xml").lineIterator.next() shouldBe prologue
    (mdDir / "files.xml").contentAsString should include("""filepath="data/text.txt""")
  }
}
