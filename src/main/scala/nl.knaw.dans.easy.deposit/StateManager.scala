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
import nl.knaw.dans.easy.deposit.Errors.{ IllegalStateTransitionException, InvalidPropertyException, PropertyNotFoundException }
import nl.knaw.dans.easy.deposit.docs.StateInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.{ ConfigurationException, PropertiesConfiguration }

import scala.util.{ Failure, Success, Try }

case class StateManager(draftDeposit: DepositDir, submitBase: File, easyHome: URL) extends DebugEnhancedLogging {

  private val depositDir = draftDeposit.bagDir.parent
  private val relativeDraftDir = s"${ draftDeposit.user }/${ draftDeposit.id }"
  private val stateDescriptionKey = "state.description"
  private val stateLabelKey = "state.label"

  /** created/destroyed when the state changes, needed to peek into SUBMITTED/UUID/deposit.properties */
  private val bagIdKey = "bag-store.bag-id"

  /** Available as side effect for properties not related to fetching or updating the state of the deposit. */
  @throws[ConfigurationException]
  val draftProps = new PropertiesConfiguration(
    (depositDir / "deposit.properties").toJava
  )

  private def triedSubmittedProps: Try[PropertiesConfiguration] = {
    getProp(bagIdKey, draftProps)
      .map(submittedId => {
        val submitDepositPropsFile = submitBase / submittedId / "deposit.properties"
        if (submitDepositPropsFile.exists)
          new PropertiesConfiguration(submitDepositPropsFile.toJava)
        else new PropertiesConfiguration // when the next request is done before the deposit is moved to `submitBase`.
      })
  }

  /** @return the state-label/description from drafts/USER/UUID/deposit.properties
   *          unless more recent values might be available in SUBMITTED/UUID/deposit.properties
   */
  def getStateInfo: Try[StateInfo] = {
    getProp(stateLabelKey, draftProps).map(State.withName).map {
      case draftState @ (State.draft | State.rejected | State.archived) =>
        StateInfo(draftState, getStateDescription(draftProps))
      case draftState @ (State.submitted | State.inProgress) =>
        triedSubmittedProps
          .map(newStateFromSubmitted(draftState, _).getOrElse(StateInfo(draftState, getStateDescription(draftProps))))
          .getOrRecover { e =>
            logger.error(e.getMessage, e)
            // saving the changed message won't change behaviour on the next call
            StateInfo(draftState, mailToDansMessage)
          }
    }
  }

  private def newStateFromSubmitted(draftState: State.State, submittedProps: PropertiesConfiguration): Option[StateInfo] = {

    def landingPage(msgStart: String): String = {
      val url = getProp("identifier.fedora", submittedProps)
        .map(id => s"$easyHome/datasets/id/$id")
        .getOrElse(s"$easyHome/mydatasets") // fall back
      s"""$msgStart <a href="$url" target="_blank">$url</a>"""
    }

    Option(submittedProps.getString(stateLabelKey, null)).map {
      case "SUBMITTED" => StateInfo(draftState, getStateDescription(draftProps))
      case "REJECTED" => getProp("curation.performed", submittedProps) match {
        case Success("yes") => saveInDraft(StateInfo(State.rejected, getStateDescription(submittedProps)))
        case _ => StateInfo(draftState, mailToDansMessage)
      }
      case "FAILED" => StateInfo(draftState, mailToDansMessage)
      case "IN_REVIEW" => saveInDraft(StateInfo(State.inProgress, landingPage("The deposit is available at")))
      case "FEDORA_ARCHIVED" |
           "ARCHIVED" => saveInDraft(StateInfo(State.archived, landingPage("The dataset is published at")))
      case str =>
        logger.error(InvalidPropertyException(stateLabelKey, str, submittedProps).getMessage)
        StateInfo(draftState, mailToDansMessage)
    }
  }

  def setMailToDansDescription(): Try[Unit] = Try {
    // using saveNewStateInDraftDeposit could inadvertently change the state
    draftProps.setProperty(stateDescriptionKey, mailToDansMessage)
    draftProps.save()
  }

  def canChangeState(oldStateInfo: StateInfo, newStateInfo: StateInfo): Try[Unit] = {
    // changeState returns the same exception but the submitter should not start without checking
    val oldState = oldStateInfo.state
    val newState = newStateInfo.state
    (oldState, newState) match {
      case (State.draft, State.submitted) |
           (State.rejected, State.draft) => Success(())
      case _ => Failure(IllegalStateTransitionException(oldState, newState))
    }
  }

  def changeState(oldStateInfo: StateInfo, newStateInfo: StateInfo): Try[Unit] = {
    logger.info(s"[${ draftDeposit.id }] changing deposit state from ${ oldStateInfo.state } to ${ newStateInfo.state } with description ${ newStateInfo.stateDescription }")

    // getStateInfo has been called by canChangeState, but it is not an IO action
    // so let's keep it simple without optimisation
    (oldStateInfo.state, newStateInfo.state) match {
      case (State.draft, State.submitted) =>
        val bagStoreBagId = UUID.randomUUID()
        // probably properly saved without toString but getSubmittedBagId would throw
        // ConversionException: 'bag-store.bag-id' doesn't map to a String object
        draftProps.setProperty(bagIdKey, bagStoreBagId.toString)
        saveInDraft(newStateInfo)
        Success(())
      case (State.rejected, State.draft) =>
        draftProps.clearProperty(bagIdKey)
        saveInDraft(newStateInfo)
        Success(())
      case (oldState, newState) =>
        Failure(IllegalStateTransitionException(oldState, newState))
    }
  }

  /** @return the value of bag-store.bag-id which is
   *          - created by changeState when changed to SUBMITTED
   *          - destroyed by changeState when changed to DRAFT
   *          fails with:
   *          PropertyException (if bag-store.bag-id is not found)
   *          ConversionException (if setProperty was called with something else than a String)
   *          IllegalArgumentException (if the value is not a valid UUID)
   */
  def getSubmittedBagId: Try[UUID] = Try {
    // exception intercepted as not expected
    // by TriedActionResult.getOrRecoverWithActionResult
    UUID.fromString(draftProps.getString(bagIdKey))
  }

  /**
   * @return a message that overrides the status message of a technical problem
   *         in easy-ingest-flow or with the preparation of its input
   */
  private def mailToDansMessage: String = {
    draftDeposit.mailToDansMessage(
      linkIntro = "Something went wrong while processing this deposit",
      bodyMsg = "Something went wrong while processing my deposit. Could you please investigate the issue?",
      ref = getSubmittedBagId.map(_.toString).getOrElse(s"DRAFT/$relativeDraftDir"),
    )
  }

  def saveInDraft(newStateInfo: StateInfo): StateInfo = {
    draftProps.setProperty(stateLabelKey, newStateInfo.state.toString)
    draftProps.setProperty(stateDescriptionKey, newStateInfo.stateDescription)
    draftProps.save()
    newStateInfo
  }

  private def getStateDescription(props: PropertiesConfiguration, default: String = ""): String = {
    props.getString(stateDescriptionKey, default)
  }

  private def getProp(key: String, props: PropertiesConfiguration): Try[String] = Try {
    Option(props.getString(key, null))
      .getOrElse(throw PropertyNotFoundException(key, props))
  }
}
