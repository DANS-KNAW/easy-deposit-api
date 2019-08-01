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
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.{ Failure, Success, Try }

case class StateManager(depositDir: File, submitBase: File, easyHome: URL) extends DebugEnhancedLogging {

  private val stateDescriptionKey = "state.description"
  private val stateLabelKey = "state.label"

  /** created/destroyed when the state changes, needed to peek into SUBMITTED/UUID/deposit.properties */
  private val bagIdKey = "bag-store.bag-id"

  /** Available as side effect for properties not related to fetching or updating the state of the deposit. */
  val draftProps = new PropertiesConfiguration(
    (depositDir / "deposit.properties").toJava
  )
  private lazy val submittedProps = new PropertiesConfiguration(
    (submitBase / getProp(bagIdKey, draftProps) / "deposit.properties").toJava
  )

  /** @return the state-label/description from drafts/USER/UUID/deposit.properties
   *          unless more recent values might be available in SUBMITTED/UUID/deposit.properties
   */
  def getStateInfo: Try[StateInfo] = Try {
    getStateInDraftDeposit match {
      case stateInDraftDeposit @ (State.submitted | State.inProgress) =>
        val stateInSubmittedDeposit: StateInfo = Try(getProp(stateLabelKey, submittedProps)) match {
          case Success("SUBMITTED") => StateInfo(State.submitted, getStateDescription(draftProps))
          case Success("REJECTED") => StateInfo(State.rejected, getStateDescription(submittedProps))
          case Success("FAILED") => StateInfo(State.inProgress, s"The deposit is in progress.")
          case Success("IN_REVIEW") => StateInfo(State.inProgress, s"""The deposit is available at <a href="$landingPage" target="_blank">$landingPage</a>""")
          case Success("FEDORA_ARCHIVED") |
               Success("ARCHIVED") => StateInfo(State.archived, s"""The dataset is published at <a href="$landingPage" target="_blank">$landingPage</a>""")
          case Success(str: String) =>
            logger.error(InvalidPropertyException(stateLabelKey, str, submittedProps).getMessage)
            StateInfo(State.inProgress, s"The deposit is in progress.")
          case Failure(e) =>
            logger.error(e.getMessage, e)
            StateInfo(stateInDraftDeposit, getStateDescription(draftProps))
        }
        saveNewStateInDraftDeposit(stateInSubmittedDeposit)
        stateInSubmittedDeposit
      case draftState =>
        StateInfo(draftState, getStateDescription(draftProps))
    }
  }

  def canChangeState(newStateInfo: StateInfo): Try[Unit] = getStateInfo.flatMap { oldStateInfo =>
    // changeState returns the same exception but the submitter should not start without checking
    val oldState = oldStateInfo.state
    val newState = newStateInfo.state
    (oldState, newState) match {
      case (State.draft, State.submitted) |
           (State.rejected, State.draft) => Success(())
      case _ => Failure(IllegalStateTransitionException(oldState, newState))
    }
  }

  def changeState(newStateInfo: StateInfo): Try[Unit] = getStateInfo.flatMap { old =>
    // getStateInfo has been called by canChangeState, but it is not an IO action
    // so let's keep it simple without optimisation
    (old.state, newStateInfo.state) match {
      case (State.draft, State.submitted) =>
        val bagStoreBagId = UUID.randomUUID()
        // probably properly saved without toString but getSubmittedBagId would throw
        // ConversionException: 'bag-store.bag-id' doesn't map to a String object
        draftProps.setProperty(bagIdKey, bagStoreBagId.toString)
        saveNewStateInDraftDeposit(newStateInfo)
        Success(())
      case (State.rejected, State.draft) =>
        draftProps.clearProperty(bagIdKey)
        saveNewStateInDraftDeposit(newStateInfo)
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

  private def landingPage = {
    Try { getProp("identifier.fedora", submittedProps) }
      .map(id => s"$easyHome/datasets/id/$id")
      .getOrElse(s"$easyHome/mydatasets") // fall back
  }

  private def saveNewStateInDraftDeposit(newStateInfo: StateInfo): Unit = {
    draftProps.setProperty(stateLabelKey, newStateInfo.state.toString)
    draftProps.setProperty(stateDescriptionKey, newStateInfo.stateDescription)
    draftProps.save()
  }

  @throws[InvalidPropertyException](s"when stateLabelKey is not found in draftProps")
  private def getStateInDraftDeposit: State.Value = {
    val str = getProp(stateLabelKey, draftProps)
    Try { State.withName(str) }
      .getOrElse(throw InvalidPropertyException(stateLabelKey, str, draftProps))
  }

  private def getStateDescription(props: PropertiesConfiguration, default: String =""): String = {
    Try(getProp(stateDescriptionKey, props)).getOrElse(default)
  }

  @throws[PropertyNotFoundException]("when key is not found in props")
  private def getProp(key: String, props: PropertiesConfiguration): String = {
    Option(props.getString(key, null))
      .getOrElse(throw PropertyNotFoundException(key, props))
  }
}
