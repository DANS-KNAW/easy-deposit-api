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

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.{ IllegalStateTransitionException, PropertyNotFoundException }
import nl.knaw.dans.easy.deposit.docs.StateInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State

import scala.util.{ Failure, Success }

class StateManagerSpec extends TestSupportFixture {
  private val draftDeposit: File = testDir / "draft"
  private val submitBase = testDir / "submitted"

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
    StateManager(draftDeposit, File("does-not-exist")).getStateInfo should matchPattern {
      case Success(StateInfo(State.draft, `message`)) =>
    }
  }

  it should "require a bag-store.bag-id for state SUBMITTED" in {
    val message = "The dataset is ready for processing"
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = $message
      """.stripMargin)
    StateManager(draftDeposit, File("does-not-exist")).getStateInfo should matchPattern {
      case Failure(e: PropertyNotFoundException) if e.getMessage ==
        s"'bag-store.bag-id' not found in $draftDeposit/deposit.properties" =>
    }
  }

  it should "require submit properties for state SUBMITTED" in {
    val message = "The dataset is ready for processing"
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = $message
         |bag-store.bag-id = $uuid
      """.stripMargin)
    StateManager(draftDeposit, testDir / "does-not-exist").getStateInfo should matchPattern {
      case Failure(e: PropertyNotFoundException) if e.getMessage ==
        s"'state.label' not found in $testDir/does-not-exist/$uuid/deposit.properties" =>
      // actually the submitted deposit does not exist
    }
  }

  it should "change SUBMITTED to IN_PROGRESS" in {
    val oldMessage = "The deposit is ready for processing"
    val newMessage = "The deposit is available at https://easy.dans.knaw.nl/ui/mydatasets"
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = $oldMessage
         |bag-store.bag-id = $uuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = IN_REVIEW
         |state.description = $newMessage
      """.stripMargin)
    StateManager(draftDeposit, submitBase).getStateInfo should matchPattern {
      case Success(StateInfo(State.inProgress, `newMessage`)) =>
    }
  }

  "setStateInfo" should "result in Success when transitioning from DRAFT to SUBMITTED" in {
    draftPropsFile.writeText(
      """state.label = DRAFT
        |state.description = Deposit is open for changes.
      """.stripMargin)
    val stateManager = StateManager(draftDeposit, submitBase)
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
    StateManager(draftDeposit, submitBase)
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
    StateManager(draftDeposit, submitBase)
      .changeState(StateInfo(State.archived, "rabarbera")) should matchPattern {
      case Failure(e: IllegalStateTransitionException) if e.getMessage == "Cannot transition from DRAFT to ARCHIVED" =>
    }
    draftPropsFile.contentAsString shouldBe props
  }
}
