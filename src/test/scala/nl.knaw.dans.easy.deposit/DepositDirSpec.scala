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
import scala.util.{ Failure, Success}

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
    val dd = DepositDir.create(draftsDir, "user001")
    dd shouldBe a[Success[_]]
    inside(dd) {
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
    inside(DepositDir.list(draftsDir, "user001")) {
      case Success(Seq()) =>
    }
  }

  it should """show one deposit of "user001" user""" in {
    DepositDir.create(draftsDir, "user001")
    inside(DepositDir.list(draftsDir, "user001")) {
      case Success(Seq(_)) =>
    }
  }

  it should """show more than two deposits of "user001" user""" in {
    for (i <- 1 to 3) DepositDir.create(draftsDir, "user001")
    inside(DepositDir.list(draftsDir, "user001")) {
      case Success(Seq(_,_,_)) =>
    }
  }

  "get" should """return a specified deposit""" in {
    DepositDir.create(draftsDir, "user001")
    val dp = DepositDir.list(draftsDir, "user001")
    inside(DepositDir.get(draftsDir, "user001", dp.get.head.id)) {
      case Success(_) =>
    }
  }

}
