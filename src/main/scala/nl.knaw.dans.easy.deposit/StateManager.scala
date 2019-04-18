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

import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.{ IllegalStateTransitionException, InvalidPropertyException, PropertyNotFoundException }
import nl.knaw.dans.easy.deposit.docs.StateInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.{ Failure, Success, Try }

case class StateManager(depositDir: File, submitBase: File) {
  private val stateDescriptionKey = "state.description"
  private val stateLabelKey = "state.label"
  private val bagIdKey = "bag-store.bag-id"
  val draftProps = new PropertiesConfiguration(
    (depositDir / "deposit.properties").toJava
  )
  private lazy val submittedProps = new PropertiesConfiguration(
    (submitBase / getProp(bagIdKey, draftProps) / "deposit.properties").toJava
  )

  def getStateInfo: Try[StateInfo] = Try {
    val draftState = getStateLabel()
    draftState match {
      case State.submitted | State.inProgress =>
        val newState: StateInfo = getProp(stateLabelKey, submittedProps) match {
          case "SUBMITTED" => StateInfo(State.submitted, getStateDescription(draftProps))
          case "REJECTED" => StateInfo(State.rejected, getStateDescription(submittedProps))
          case "FEDORA_ARCHIVED" => StateInfo(State.archived, "The dataset is published in https://easy.dans.knaw.nl/ui")
          case "IN_REVIEW" => StateInfo(State.inProgress, "The dataset is visible for you under your datasets in https://easy.dans.knaw.nl/ui")
          case "FAILED" => StateInfo(State.inProgress, "The dataset passed automated validations")
          case str: String => throw InvalidPropertyException(stateLabelKey, str, submittedProps)
        }
        saveNewState(newState)
      case State.draft | State.rejected | State.archived =>
        StateInfo(draftState, getStateDescription(draftProps))
    }
  }

  def canChangeState(newStateInfo: StateInfo): Try[Unit] = getStateInfo.flatMap { oldStateInfo =>
    val oldState = oldStateInfo.state
    val newState = newStateInfo.state
    (oldState, newState) match {
      case (State.draft, State.submitted) |
           (State.rejected, State.draft) => Success(())
      case _ => Failure(IllegalStateTransitionException(oldState, newState))
    }
  }

  def changeState(newStateInfo: StateInfo): Try[Unit] = getStateInfo.flatMap { old =>
    // getStateInfo has been called by canChangeState, but it is not an IO action so no optimisation
    (old.state, newStateInfo.state) match {
      case (State.draft, State.submitted) =>
        val bagStoreBagId = UUID.randomUUID()
        draftProps.setProperty(bagIdKey, bagStoreBagId.toString)
        saveNewState(newStateInfo)
        Success(())
      case (State.rejected, State.draft) =>
        draftProps.clearProperty(bagIdKey)
        saveNewState(newStateInfo)
        Success(())
      case (oldState, newState) =>
        Failure(IllegalStateTransitionException(oldState, newState))
    }
  }

  /** @return the value of bag-store.bag-id created by changeState when set to SUBMITTED */
  def getSubmittedBagId: Try[UUID] = Try {
    // exception intercepted as not expected
    // by TriedActionResult.getOrRecoverWithActionResult
    UUID.fromString(draftProps.getString(bagIdKey))
  }

  private def saveNewState(newStateInfo: StateInfo) = {
    draftProps.setProperty(stateLabelKey, newStateInfo.state.toString)
    draftProps.setProperty(stateDescriptionKey, newStateInfo.stateDescription)
    draftProps.save()
    newStateInfo
  }

  private def getStateLabel(props: PropertiesConfiguration = draftProps): State = {
    Option(props.getString(stateLabelKey))
      .map(toDraftState) // TODO recover InvalidPropertyException
      .getOrElse(throw PropertyNotFoundException(stateLabelKey, props))
  }

  private def toDraftState(str: String) = Try {
    State.withName(str)
  }.getOrElse(throw InvalidPropertyException(stateLabelKey, str, draftProps))

  private def getStateDescription(props: PropertiesConfiguration): String = {
    getProp(stateDescriptionKey, props)
  }

  private def getProp(key: String, props: PropertiesConfiguration): String = {
    Option(props.getString(key))
      .getOrElse(throw PropertyNotFoundException(key, props))
  }
}
