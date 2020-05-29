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
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.IllegalStateTransitionException
import nl.knaw.dans.easy.deposit.docs.StateInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.properties.DepositProperties.SubmittedProperties
import nl.knaw.dans.easy.deposit.properties.{ DepositProperties, DepositPropertiesRepository }

import scala.util.{ Failure, Success }

class StateManagerSpec extends TestSupportFixture {

  // need hard coded UUIDs to manually try the expectedURL-s in a browser
  override lazy val uuid: UUID = UUID.fromString("7fa835ce-0987-4064-90ca-a7b75ce78a16")
  private val submittedUuid = UUID.fromString("a890ad74-872b-4f21-81a8-f3ef88b944ba")

  private val draftDeposit: DepositDir = DepositDir(testDir / "draft", "foo", uuid)
  private val easyHome: URL = new URL("https://easy.dans.knaw.nl/ui")

  private def draftPropsFile: File = draftDeposit.bagDir.createDirectories().parent / "deposit.properties"

  private val repo: DepositPropertiesRepository = mock[DepositPropertiesRepository]
  private val properties: DepositProperties = mock[DepositProperties]

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
    repo.load _ expects * never()

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(State.draft, message))
  }

  it should "not stumble on missing draft property bag-store.bag-id for state SUBMITTED" in {
    // the problem will be logged
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
      """.stripMargin)
    repo.load _ expects * never()

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(
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
    repo.load _ expects submittedUuid returning Success(properties)
    properties.getSubmittedProperties _ expects () returning Failure(new Exception("No state available for deposit"))

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.submitted,
      mailtoMessage(s"""mailto:info@dans.knaw.nl?subject=Deposit%20processing%20error:%20%20reference%20$submittedUuid&body=Dear%20data%20manager%2C%0A%0ASomething%20went%20wrong%20while%20processing%20my%20deposit.%20Could%20you%20please%20investigate%20the%20issue?%0A%0ADataset%20reference:%0A%20%20%20$submittedUuid%0ATitle:%0A%20%20%20%0A%0AKind%20regards%2C%0Afoo%0A"""),
    ))
  }

  it should "search for deposit.properties file if the deposit is not yet registered in the ServiceDepositRepository" in {
    // the problem will be logged
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    repo.load _ expects submittedUuid returning Success(properties)
    properties.getSubmittedProperties _ expects () returning Success(SubmittedProperties(submittedUuid, null, null, false, None))

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(
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
    repo.load _ expects submittedUuid returning Success(properties)
    properties.getSubmittedProperties _ expects() returning Success(SubmittedProperties(submittedUuid, "IN_REVIEW", "rabarbera", curationPerformed = false, None))

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(
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
    repo.load _ expects submittedUuid returning Success(properties)
    properties.getSubmittedProperties _ expects() returning Success(SubmittedProperties(submittedUuid, "IN_REVIEW", "rabarbera", curationPerformed = false, Some("easy-dataset:1239")))

    val testResult = StateManager(draftDeposit, repo, easyHome).getStateInfo
    testResult shouldBe Success(StateInfo(
      State.inProgress,
      """The deposit is available at <a href="https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:1239" target="_blank">https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:1239</a>"""
    ))
  }

  it should "mail the draft uuid" in {
    draftPropsFile.writeText(
      """state.label = SUBMITTED
        |state.description = The deposit is being processed""".stripMargin)
    ((draftDeposit.bagDir / "metadata").createDirectories() / "dataset.json")
      .write("""{"titles":["A test with a title longer than forty-two characters."]}""")
    repo.load _ expects * never()

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(
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
    ((draftDeposit.bagDir / "metadata").createDirectories() / "dataset.json")
      .write(
        """{"titles":["
          | A test with
          | new lines and html <a href='http://user.hack.dans.knaw.nl'>link</a>.
          |
          | "]}""".stripMargin)
    repo.load _ expects submittedUuid returning Success(properties)
    properties.getSubmittedProperties _ expects() returning Success(SubmittedProperties(submittedUuid, "REJECTED", null, curationPerformed = false, None))

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(
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
    repo.load _ expects submittedUuid returning Success(properties)
    properties.getSubmittedProperties _ expects() returning Success(SubmittedProperties(submittedUuid, "REJECTED", "rabarbera", curationPerformed = true, None))

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(State.rejected, """rabarbera"""))
  }

  it should "return a generic landing page when no fedora-id is available" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is being processed
         |bag-store.bag-id = $submittedUuid
         |identifier.doi = 10.5072/dans-zyf-v9sc
      """.stripMargin)
    repo.load _ expects submittedUuid returning Success(properties)
    properties.getSubmittedProperties _ expects() returning Success(SubmittedProperties(submittedUuid, "FEDORA_ARCHIVED", "rabarbera", curationPerformed = false, None))

    StateManager(draftDeposit, repo, easyHome).getStateInfo shouldBe Success(StateInfo(
      State.archived,
      """The dataset is published at <a href="https://easy.dans.knaw.nl/ui/mydatasets" target="_blank">https://easy.dans.knaw.nl/ui/mydatasets</a>"""
    ))
  }

  "setStateInfo" should "result in Success when transitioning from DRAFT to SUBMITTED" in {
    repo.load _ expects * never()

    val stateManager = StateManager(draftDeposit, repo, easyHome)
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
    repo.load _ expects * never()

    val stateManager = StateManager(draftDeposit, repo, easyHome)
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
    repo.load _ expects * never()

    val props =
      s"""state.label = DRAFT
         |state.description = user is working on this
         |bag-store.bag-id = $submittedUuid
         |""".stripMargin
    draftPropsFile.writeText(props)
    val stateManager = StateManager(draftDeposit, repo, easyHome)
    stateManager.changeState(
      oldStateInfo = StateInfo(State.draft, "user is working on this"),
      newStateInfo = StateInfo(State.archived, "rabarbera"),
    ) should matchPattern {
      case Failure(e: IllegalStateTransitionException) if e.getMessage == "Cannot transition from DRAFT to ARCHIVED" =>
    }
    draftPropsFile.contentAsString shouldBe props
  }
}
