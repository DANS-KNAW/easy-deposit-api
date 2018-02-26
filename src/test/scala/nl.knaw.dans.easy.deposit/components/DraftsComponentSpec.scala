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
package nl.knaw.dans.easy.deposit.components

import better.files.File
import better.files.File._
import nl.knaw.dans.easy.deposit.TestSupportFixture

class DraftsComponentSpec extends TestSupportFixture {

  private val testDrafts = testDir / "drafts"
  private val drafts = new DraftsComponent {
    val draftRoot: File = testDrafts
  }

  override def beforeEach(): Unit = {
    super.beforeEach()

    (currentWorkingDirectory / "src" / "test" / "resources" / "drafts").copyTo(testDrafts)
  }

  "listDrafts" should "list all drafts of a specific user" ignore {
    drafts.listDrafts(User("user001")) shouldBe Seq(
//      DraftInformation(...)
    )
  }

  it should "return an empty list if the user has no drafts"

  it should "return an empty list when the user does not exist in the drafts area"
}
