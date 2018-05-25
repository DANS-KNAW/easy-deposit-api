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

import scala.util.{ Failure, Success }

class DepositDirDatasetMetadataSpec extends TestSupportFixture {
  private val dd = DepositDir(testDir / "drafts", "foo", uuid)
  private val metadataFile = dd.baseDir / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    dd.baseDir.createDirectories()
  }

  "set" should "create a file" in {
    // prepare empty deposit
    metadataFile.parent.createIfNotExists(asDirectory = true, createParents = true)

    dd.writeDatasetMetadataJson(DatasetMetadata()) shouldBe Success(())
    metadataFile.contentAsString shouldBe
      """{"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
  }

  it should "overwrite existing metadata" in {
    // prepare existing metadata
    metadataFile.parent.createIfNotExists(asDirectory = true, createParents = true)
    metadataFile.write("blabla")

    dd.writeDatasetMetadataJson(DatasetMetadata()) shouldBe Success(())
    metadataFile.contentAsString shouldBe
      """{"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
  }

  it should "report the deposit does not exist" in {
    // no preparations for a "lost" deposit
    dd.writeDatasetMetadataJson(DatasetMetadata()) should matchPattern {
      case Failure(NoSuchDepositException(_, _, _)) =>
    }
  }

  "get" should "complain about a not found file" in {
    dd.getDatasetMetadata should matchPattern {
      case Failure(NoSuchDepositException(_, _, _)) =>
    }
  }
  it should "get the content" in {
    metadataFile.parent.createIfNotExists(asDirectory = true, createParents = true)
    metadataFile.write("{}")
    dd.getDatasetMetadata shouldBe Success(DatasetMetadata())
  }
  it should "complain about invalid content" in {
    metadataFile.parent.createIfNotExists(asDirectory = true, createParents = true)
    metadataFile.write("---")
    dd.getDatasetMetadata should matchPattern {
      case Failure(CorruptDepositException(_, _, _)) =>
    }
  }
}
