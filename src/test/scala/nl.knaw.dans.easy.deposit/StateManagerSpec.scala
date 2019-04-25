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

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.{ IllegalStateTransitionException, PropertyNotFoundException }
import nl.knaw.dans.easy.deposit.docs.StateInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State

import scala.util.{ Failure, Success }

class StateManagerSpec extends TestSupportFixture {
  private val draftDeposit: File = testDir / "draft"
  private val submitBase = testDir / "submitted"
  private val easyHome: URL = new URL("https://easy.dans.knaw.nl/ui")

  private def submittedPropsFile = (submitBase / uuid.toString).createDirectories() / "deposit.properties"

  private def draftPropsFile = draftDeposit.createDirectories() / "deposit.properties"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "getStateInfo" should "not access submitted properties for state DRAFT" in {
    val message = "Deposit is open for changes."
    draftPropsFile.writeText(
      s"""state.label = DRAFT
         |state.description = $message
      """.stripMargin)
    StateManager(draftDeposit, File("does-not-exist"), easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.draft, `message`)) =>
    }
  }

  it should "require a bag-store.bag-id for state SUBMITTED" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The dataset is ready for processing
      """.stripMargin)
    StateManager(draftDeposit, File("does-not-exist"), easyHome).getStateInfo should matchPattern {
      case Failure(e: PropertyNotFoundException) if e.getMessage ==
        s"'bag-store.bag-id' not found in $draftDeposit/deposit.properties" =>
    }
  }

  it should "require submit properties for state SUBMITTED" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The dataset is ready for processing
         |bag-store.bag-id = $uuid
      """.stripMargin)
    StateManager(draftDeposit, testDir / "does-not-exist", easyHome).getStateInfo should matchPattern {
      case Failure(e: PropertyNotFoundException) if e.getMessage ==
        s"'state.label' not found in $testDir/does-not-exist/$uuid/deposit.properties" =>
      // actually the submitted deposit does not exist
      // for example due to lack of space to create a copy in the staging area
      // or removed by some ignorant clean-up action
    }
  }

  it should "change SUBMITTED to IN_PROGRESS" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processing
         |bag-store.bag-id = $uuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = IN_REVIEW
         |state.description = rabarbera
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.inProgress, "The deposit is available at https://easy.dans.knaw.nl/ui/mydatasets")) =>
    }
  }

  it should "return the fedora landing page" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processing
         |bag-store.bag-id = $uuid
         |identifier.doi = 10.5072/dans-zyf-v9sc
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = IN_REVIEW
         |state.description = rabarbera
         |identifier.fedora = easy-dataset:1239
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.inProgress, "The deposit is available at https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:1239")) =>
    }
  }

  it should "return a generic landing page when no fedora-id is available" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processin
         |bag-store.bag-id = $uuid
         |identifier.doi = 10.5072/dans-zyf-v9sc
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = FEDORA_ARCHIVED
         |state.description = rabarbeara
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.archived, "The dataset is published at https://easy.dans.knaw.nl/ui/mydatasets")) =>
    }
  }

  "setStateInfo" should "result in Success when transitioning from DRAFT to SUBMITTED" in {
    draftPropsFile.writeText(
      """state.label = DRAFT
        |state.description = Deposit is open for changes.
      """.stripMargin)
    val stateManager = StateManager(draftDeposit, submitBase, easyHome)
    stateManager.changeState(StateInfo(State.submitted, "rabarbera")) shouldBe a[Success[_]]
    val props = draftPropsFile.contentAsString
    props should include("bag-store.bag-id = ")
    props.split("\n") should contain allOf(
      "state.label = SUBMITTED",
      "state.description = rabarbera"
    )
  }

  it should "result in Success when transitioning from REJECTED to DRAFT" in {
    draftPropsFile.writeText(
      s"""state.label = REJECTED
         |state.description = Something's rotten on the state of ...
         |bag-store.bag-id = $uuid
         |""".stripMargin)
    StateManager(draftDeposit, submitBase, easyHome)
      .changeState(StateInfo(State.draft, "rabarbera")) shouldBe a[Success[_]]
    draftPropsFile.contentAsString shouldBe
      s"""state.label = DRAFT
         |state.description = rabarbera
         |""".stripMargin
  }

  it should "result in IllegalStateTransitionException when transitioning from DRAFT to ARCHIVED" in {
    val props =
      s"""state.label = DRAFT
         |state.description = Something's rotten on the state of ...
         |bag-store.bag-id = $uuid
         |""".stripMargin
    draftPropsFile.writeText(props)
    StateManager(draftDeposit, submitBase, easyHome)
      .changeState(StateInfo(State.archived, "rabarbera")) should matchPattern {
      case Failure(e: IllegalStateTransitionException) if e.getMessage == "Cannot transition from DRAFT to ARCHIVED" =>
    }
    draftPropsFile.contentAsString shouldBe props
  }
}
