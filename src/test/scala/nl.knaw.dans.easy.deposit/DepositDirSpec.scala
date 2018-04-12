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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata

import scala.util.{ Failure, Success }

class DepositDirSpec extends TestSupportFixture {
  before {
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

  private val dd = DepositDir(draftsDir, "foo", uuid)
  private val metadataFile = dd.baseDir / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json"

  "setDatasetMetadata" should "create a file" in {
    // prepare empty deposit
    metadataFile.parent.createIfNotExists(asDirectory = true, createParents = true)

    dd.setDatasetMetadata(DatasetMetadata()) shouldBe Success(())
    metadataFile.contentAsString shouldBe
      """{"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
  }

  it should "overwrite existing metadata" in {
    // prepare existing metadata
    metadataFile.parent.createIfNotExists(asDirectory = true, createParents = true)
    metadataFile.write("blabla")

    dd.setDatasetMetadata(DatasetMetadata()) shouldBe Success(())
    metadataFile.contentAsString shouldBe
      """{"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
  }

  it should "report the deposit does not exist" in {
    // no preparations for a "lost" deposit
    dd.setDatasetMetadata(DatasetMetadata()) should matchPattern {
      case Failure(NoSuchDepositException(_, _, _)) =>
    }
  }

  "getDatasetMetadata" should "complain about a not found file" in {
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
