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

import scala.util.{ Failure, Success }

class StateManagerSpec extends TestSupportFixture {

  // need a hard coded value for a pattern match and to manually try the expected mailto href value
  override lazy val uuid: UUID = UUID.fromString("7fa835ce-0987-4064-90ca-a7b75ce78a16")
  private val draftDeposit: DepositDir = DepositDir(testDir / "draft", "foo", uuid)

  private val submittedUuid = UUID.fromString("a890ad74-872b-4f21-81a8-f3ef88b944ba")
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
    StateManager(draftDeposit, File("does-not-exist"), easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.draft, `message`)) =>
    }
  }

  it should "not stumble on missing draft property bag-store.bag-id for state SUBMITTED" in {
    // the problem will be logged
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The dataset is ready for processing
      """.stripMargin)
    StateManager(draftDeposit, File("does-not-exist"), easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.submitted, """Something went wrong while processing this deposit. Please <a href="mailto:info@dans.knaw.nl?subject=%20%28reference%20nr:%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16%29&body=Hello%0A%0ACould%20you%20please%20figure%20out%20what%20went%20wrong%20with%20my%20deposit?%0A%0AIt%20has%20reference:%0A%20%20%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16%0Aand%20title:%0A%20%20%20%0A">contact DANS</a>""")) =>
    }
  }

  it should "not stumble on missing ingest-flow-inbox state property for state SUBMITTED" in {
    // the problem will be logged
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The dataset is ready for processing
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    StateManager(draftDeposit, testDir / "does-not-exist", easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.submitted, """Something went wrong while processing this deposit. Please <a href="mailto:info@dans.knaw.nl?subject=%20%28reference%20nr:%20a890ad74-872b-4f21-81a8-f3ef88b944ba%29&body=Hello%0A%0ACould%20you%20please%20figure%20out%20what%20went%20wrong%20with%20my%20deposit?%0A%0AIt%20has%20reference:%0A%20%20%20a890ad74-872b-4f21-81a8-f3ef88b944ba%0Aand%20title:%0A%20%20%20%0A">contact DANS</a>""")) =>
    }
  }

  it should "change SUBMITTED to IN_PROGRESS" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processing
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = IN_REVIEW
         |state.description = rabarbera
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.inProgress, """The deposit is available at <a href="https://easy.dans.knaw.nl/ui/mydatasets" target="_blank">https://easy.dans.knaw.nl/ui/mydatasets</a>""")) =>
    }
  }

  it should "return the fedora landing page" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processing
         |bag-store.bag-id = $submittedUuid
         |identifier.doi = 10.5072/dans-zyf-v9sc
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = IN_REVIEW
         |state.description = rabarbera
         |identifier.fedora = easy-dataset:1239
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.inProgress, """The deposit is available at <a href="https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:1239" target="_blank">https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:1239</a>""")) =>
    }
  }

  it should "mail the draft uuid" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processing
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = REJECTED
         |curation.performed = no
         |state.description = rabarbera
         |identifier.fedora = easy-dataset:1239
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.submitted, """Something went wrong while processing this deposit. Please <a href="mailto:info@dans.knaw.nl?subject=%20%28reference%20nr:%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16%29&body=Hello%0A%0ACould%20you%20please%20figure%20out%20what%20went%20wrong%20with%20my%20deposit?%0A%0AIt%20has%20reference:%0A%20%20%20DRAFT/foo/7fa835ce-0987-4064-90ca-a7b75ce78a16%0Aand%20title:%0A%20%20%20%0A">contact DANS</a>""")) =>
    }
  }

  it should "mail the submitted uuid" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processing
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = REJECTED
         |curation.performed = no
      """.stripMargin)
    ((draftDeposit.bagDir / "metadata").createDirectories() / "dataset.json")
      .write(
        """{"titles":["
          | A complete ground hog <a href='mailto:info@dans.knaw.nl'>weather</a> report
          | is a way too long title for the mail to dans message.
          |
          | Lorem ipsum dolor sit amet, consectetur adipiscing elit,
          | sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
          | Dolor sed viverra ipsum nunc aliquet bibendum enim.
          | In massa tempor nec feugiat. Nunc aliquet bibendum enim facilisis gravida.
          | Nisl nunc mi ipsum faucibus vitae aliquet nec ullamcorper.
          | Amet luctus venenatis lectus magna fringilla.
          | Volutpat maecenas volutpat blandit aliquam etiam erat velit scelerisque in.
          | Egestas egestas fringilla phasellus faucibus scelerisque eleifend.
          | Sagittis orci a scelerisque purus semper eget duis.
          | Nulla pharetra diam sit amet nisl suscipit.
          | Sed adipiscing diam donec adipiscing tristique risus nec feugiat in.
          | Fusce ut placerat orci nulla. Pharetra vel turpis nunc eget lorem dolor.
          | Tristique senectus et netus et malesuada.
          | Etiam tempor orci eu lobortis elementum nibh tellus molestie.
          | Neque egestas congue quisque egestas.
          | Egestas integer eget aliquet nibh praesent tristique.
          | Vulputate mi sit amet mauris. Sodales neque sodales ut etiam sit.
          | Dignissim suspendisse in est ante in. Volutpat commodo sed egestas egestas.
          | Felis donec et odio pellentesque diam. Pharetra vel turpis nunc eget lorem dolor sed viverra.
          | Porta nibh venenatis cras sed felis eget. Aliquam ultrices sagittis orci a.
          | Dignissim diam quis enim lobortis. Aliquet porttitor lacus luctus accumsan.
          | Dignissim convallis aenean et tortor at risus viverra adipiscing at.
          | "]}""".stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.submitted, """Something went wrong while processing this deposit. Please <a href="mailto:info@dans.knaw.nl?subject=A%20complete%20ground%20hog%20%3Ca%20href%3D%27mailto:info%20%28reference%20nr:%20a890ad74-872b-4f21-81a8-f3ef88b944ba%29&body=Hello%0A%0ACould%20you%20please%20figure%20out%20what%20went%20wrong%20with%20my%20deposit?%0A%0AIt%20has%20reference:%0A%20%20%20a890ad74-872b-4f21-81a8-f3ef88b944ba%0Aand%20title:%0A%20%20%20A%20complete%20ground%20hog%20%3Ca%20href%3D%27mailto:info@dans.knaw.nl%27%3Eweather%3C/a%3E%20report%0A%20is%20a%20way%20too%20long%20title%20for%20the%20mail%20to%20dans%20message.%0A%0A%20Lorem%20ipsum%20dolor%20sit%20amet%2C%20consectetur%20adipiscing%20elit%2C%0A%20sed%20do%20eiusmod%20tempor%20incididunt%20ut%20labore%20et%20dolore%20magna%20aliqua.%0A%20Dolor%20sed%20viverra%20ipsum%20nunc%20aliquet%20bibendum%20enim.%0A%20In%20massa%20tempor%20nec%20feugiat.%20Nunc%20aliquet%20bibendum%20enim%20facilisis%20gravida.%0A%20Nisl%20nunc%20mi%20ipsum%20faucibus%20vitae%20aliquet%20nec%20ullamcorper.%0A%20Amet%20luctus%20venenatis%20lectus%20magna%20fringilla.%0A%20Volutpat%20maecenas%20volutpat%20blandit%20aliquam%20etiam%20erat%20velit%20scelerisque%20in.%0A%20Egestas%20egestas%20fringilla%20phasellus%20faucibus%20scelerisque%20eleifend.%0A%20Sagittis%20orci%20a%20scelerisque%20purus%20semper%20eget%20duis.%0A%20Nulla%20pharetra%20diam%20sit%20amet%20nisl%20suscipit.%0A%20Sed%20adipiscing%20diam%20donec%20adipiscing%20tristique%20risus%20nec%20feugiat%20in.%0A%20Fusce%20ut%20placerat%20orci%20nulla.%20Pharetra%20vel%20turpis%20nunc%20eget%20lorem%20dolor.%0A%20Tristique%20senectus%20et%20netus%20et%20malesuada.%0A%20Etiam%20tempor%20orci%20eu%20lobortis%20elementum%20nibh%20tellus%20molestie.%0A%20Neque%20egesta%0A">contact DANS</a>""")) =>
    }
  }

  it should "return the curators message" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processing
         |bag-store.bag-id = $submittedUuid
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = REJECTED
         |curation.performed = yes
         |state.description = rabarbera
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.inProgress, """rabarbera""")) =>
    }
  }

  it should "return a generic landing page when no fedora-id is available" in {
    draftPropsFile.writeText(
      s"""state.label = SUBMITTED
         |state.description = The deposit is ready for processin
         |bag-store.bag-id = $submittedUuid
         |identifier.doi = 10.5072/dans-zyf-v9sc
      """.stripMargin)
    submittedPropsFile.writeText(
      s"""state.label = FEDORA_ARCHIVED
         |state.description = rabarbeara
      """.stripMargin)
    StateManager(draftDeposit, submitBase, easyHome).getStateInfo should matchPattern {
      case Success(StateInfo(State.archived, """The dataset is published at <a href="https://easy.dans.knaw.nl/ui/mydatasets" target="_blank">https://easy.dans.knaw.nl/ui/mydatasets</a>""")) =>
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
         |bag-store.bag-id = $submittedUuid
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
         |bag-store.bag-id = $submittedUuid
         |""".stripMargin
    draftPropsFile.writeText(props)
    StateManager(draftDeposit, submitBase, easyHome)
      .changeState(StateInfo(State.archived, "rabarbera")) should matchPattern {
      case Failure(e: IllegalStateTransitionException) if e.getMessage == "Cannot transition from DRAFT to ARCHIVED" =>
    }
    draftPropsFile.contentAsString shouldBe props
  }
}
