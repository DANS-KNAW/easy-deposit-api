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

import scala.util.Success

class DepositDirSpec extends TestSupportFixture {
  private val draftsDir = testDir / "drafts"
  draftsDir.createDirectory

  "create" should "create a new directory with deposit.properties" in  {
    val dd = DepositDir.create(draftsDir, "user001")
    dd shouldBe a[Success[_]]
    inside(dd) {
      case Success(d) =>
        (draftsDir / "user001" / d.id.toString).toJava should exist
        (draftsDir / "user001" / d.id.toString / "deposit.properties").toJava should exist
        (draftsDir / "user001" / d.id.toString / "bag").toJava should exist
        (draftsDir / "user001" / d.id.toString / "bag/bag-info.txt").toJava should exist
        (draftsDir / "user001" / d.id.toString / "bag/bagit.txt").toJava should exist
        (draftsDir / "user001" / d.id.toString / "bag/manifest-sha1.txt").toJava should exist
        (draftsDir / "user001" / d.id.toString / "bag/tagmanifest-sha1.txt").toJava should exist
        (draftsDir / "user001" / d.id.toString / "bag/data").toJava should exist
        (draftsDir / "user001" / d.id.toString / "bag/metadata").toJava should exist
    }
  }

}
