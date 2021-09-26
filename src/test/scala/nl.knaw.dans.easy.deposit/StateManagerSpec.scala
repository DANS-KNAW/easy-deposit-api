/*
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
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.IllegalStateTransitionException
import nl.knaw.dans.easy.deposit.docs.StateInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State

import scala.util.{ Failure, Success }

class StateManagerSpec extends TestSupportFixture {

  // need hard coded UUIDs to manually try the expectedURL-s in a browser
  override lazy val uuid: UUID = UUID.fromString("7fa835ce-0987-4064-90ca-a7b75ce78a16")
  private val submittedUuid = UUID.fromString("a890ad74-872b-4f21-81a8-f3ef88b944ba")

  private val draftDeposit: DepositDir = DepositDir(testDir / "draft", "foo", uuid)
  private val submitBase = testDir / "submitted"
  private val easyHome: URL = new URL("https://easy.dans.knaw.nl/ui")

  private def submittedPropsFile: File = (submitBase / submittedUuid.toString).createDirectories() / "deposit.properties"

  private def draftPropsFile: File = draftDeposit.bagDir.createDirectories().parent / "deposit.properties"

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
    StateManager(draftDeposit, File("does-not-exist"), easyHome).getStateInfo shouldBe Success(StateInfo(State.draft, message))
  }

  it should "not stumble on missing draft property bag-store.bag-id for state SUBMITTED" in {
    // the problem will be logged
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.submitted,
      mailtoMessage(s"""mailto:info@dans.knaw.nl?subject=Deposit%20processing%20error:%20%20reference%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16&body=Dear%20data%20manager%2C%0A%0ASomething%20went%20wrong%20while%20processing%20my%20deposit.%20Could%20you%20please%20investigate%20the%20issue?%0A%0ADataset%20reference:%0A%20%20%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16%0ATitle:%0A%20%20%20%0A%0AKind%20regards%2C%0Afoo%0A""")
    ))
  }

  it should "not stumble on missing ingest-flow-inbox state property for state SUBMITTED" in {
    // the problem will be logged
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.submitted,
      "The deposit is being processed",
    ))
  }

  it should "change SUBMITTED to IN_PROGRESS" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = IN_REVIEW
         |state.description = rabarbera
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.inProgress,
      """The deposit is available at <a href="https://easy.dans.knaw.nl/ui/mydatasets" target="_blank">https://easy.dans.knaw.nl/ui/mydatasets</a>"""
    ))
  }

  it should "return the fedora landing page" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
         |identifier.doi = 10.5072/dans-zyf-v9sc
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = IN_REVIEW
         |state.description = rabarbera
         |identifier.fedora = easy-dataset:1239
      """.stripMargin)
    val testResult = StateManager(draftDeposit, submitBase, easyHome).getStateInfo
    testResult shouldBe Success(StateInfo(
      State.inProgress,
      """The deposit is available at <a href="https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:1239" target="_blank">https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:1239</a>"""
    ))
  }

  it should "mail the draft uuid" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = REJECTED
         |curation.performed = no
         |state.description = rabarbera
         |identifier.fedora = easy-dataset:1239
      """.stripMargin)
    ((draftDeposit.bagDir / "metadata").createDirectories() / "dataset.json")
      .write("""{"titles":["A test with a title longer than forty-two characters."]}""")
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.submitted,
      mailtoMessage("""mailto:info@dans.knaw.nl?subject=Deposit%20processing%20error:%20A%20test%20with%20a%20title%20longer%20than%20forty-two%20%E2%80%A6%20reference%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16&body=Dear%20data%20manager%2C%0A%0ASomething%20went%20wrong%20while%20processing%20my%20deposit.%20Could%20you%20please%20investigate%20the%20issue?%0A%0ADataset%20reference:%0A%20%20%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16%0ATitle:%0A%20%20%20A%20test%20with%20a%20title%20longer%20than%20forty-two%20%E2%80%A6%0A%0AKind%20regards%2C%0Afoo%0A""")
    ))
  }

  it should "mail the submitted uuid" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = REJECTED
         |curation.performed = no
      """.stripMargin)
    ((draftDeposit.bagDir / "metadata").createDirectories() / "dataset.json")
      .write(
        """{"titles":["
          | A test with
          | new lines and html <a href='http://user.hack.dans.knaw.nl'>link</a>.
          |
          | "]}""".stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.submitted,
      mailtoMessage("""mailto:info@dans.knaw.nl?subject=Deposit%20processing%20error:%20A%20test%20with%20%20new%20lines%20and%20html%20%E2%80%A6%20reference%20a890ad74-872b-4f21-81a8-f3ef88b944ba&body=Dear%20data%20manager%2C%0A%0ASomething%20went%20wrong%20while%20processing%20my%20deposit.%20Could%20you%20please%20investigate%20the%20issue?%0A%0ADataset%20reference:%0A%20%20%20a890ad74-872b-4f21-81a8-f3ef88b944ba%0ATitle:%0A%20%20%20A%20test%20with%20%20new%20lines%20and%20html%20%E2%80%A6%0A%0AKind%20regards%2C%0Afoo%0A""")
    ))
  }

  private def mailtoMessage(expectedURL: String) = {
    s"""Something went wrong while processing this deposit. Please <a href="$expectedURL">contact DANS</a>"""
  }

  it should "return the curators message" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = REJECTED
         |curation.performed = yes
         |state.description = rabarbera
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo shouldBe Success(StateInfo(State.rejected, """rabarbera"""))
  }

  it should "return a generic landing page when no fedora-id is available" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
         |identifier.doi = 10.5072/dans-zyf-v9sc
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = FEDORA_ARCHIVED
         |state.description = rabarbeara
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.archived,
      """The dataset is published at <a href="https://easy.dans.knaw.nl/ui/mydatasets" target="_blank">https://easy.dans.knaw.nl/ui/mydatasets</a>"""
    ))
  }

  "setStateInfo" should "result in Success when transitioning from DRAFT to SUBMITTED" in {
    val stateManager = StateManager(draftDeposit, submitBase, easyHome)
    stateManager.changeState(
      oldStateInfo = StateInfo(State.draft, "Deposit is open for changes."),
      newStateInfo = StateInfo(State.submitted, "rabarbera"),
    ) shouldBe a[Success[_]]
    val props = draftPropsFile.contentAsString
    props should include("bag-store.bag-id = ")
    props.split("\n") should contain allOf(
      "state.label = SUBMITTED",
      "state.description = rabarbera"
    )
  }

  it should "result in Success when transitioning from REJECTED to DRAFT" in {
    val stateManager = StateManager(draftDeposit, submitBase, easyHome)
    stateManager.changeState(
      oldStateInfo = StateInfo(State.rejected, "Something's rotten on the state of ..."),
      newStateInfo = StateInfo(State.draft, "rabarbera"),
    ) shouldBe a[Success[_]]
    draftPropsFile.contentAsString shouldBe
      s"""state.label = DRAFT
         |state.description = rabarbera
         |""".stripMargin
  }

  it should "result in IllegalStateTransitionException when transitioning from DRAFT to ARCHIVED" in {
    val props =
      s"""state.label = DRAFT
         |state.description = user is working on this
         |bag-store.bag-id = $submittedUuid
         |""".stripMargin
    draftPropsFile.writeText(props)
    val stateManager = StateManager(draftDeposit, submitBase, easyHome)
    stateManager.changeState(
      oldStateInfo = StateInfo(State.draft, "user is working on this"),
      newStateInfo = StateInfo(State.archived, "rabarbera"),
    ) should matchPattern {
      case Failure(e: IllegalStateTransitionException) if e.getMessage == "Cannot transition from DRAFT to ARCHIVED" =>
    }
    draftPropsFile.contentAsString shouldBe props
  }
}
