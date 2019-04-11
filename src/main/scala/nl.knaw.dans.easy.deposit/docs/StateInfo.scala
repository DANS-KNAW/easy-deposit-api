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
package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.Errors.IllegalDepositStateException
import nl.knaw.dans.easy.deposit.docs.JsonUtil.RichJsonInput
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import org.json4s.JsonInput

import scala.collection.Seq
import scala.util.{ Failure, Success, Try }

case class StateInfo(state: State, stateDescription: String) {
  def canDelete: Try[Unit] = can("delete", StateInfo.deletableStates)

  def canUpdate: Try[Unit] = can("update", StateInfo.updatableStates)

  private def can(action: String, states: Seq[State]): Try[Unit] = {
    if (states.contains(state)) Success(())
    else Failure(IllegalDepositStateException(action, state, states))
  }
}

object StateInfo {
  val deletableStates: Seq[State] = Seq(State.draft, State.archived, State.rejected)
  val updatableStates: Seq[State] = Seq(State.draft, State.rejected)

  object State extends Enumeration {
    type State = Value
    val draft: State = Value("DRAFT")
    val submitted: State = Value("SUBMITTED")
    val inProgress: State = Value("IN_PROGRESS")
    val rejected: State = Value("REJECTED")
    val archived: State = Value("ARCHIVED")
  }

  implicit class StateExtensions(val state: State) extends AnyVal {
    def canChangeTo(newValue: State): Boolean = {
      (state, newValue) match {
        case (State.draft, State.submitted) => true
        case (State.rejected, State.draft) => true
        case _ => false
      }
    }
  }

  def apply(input: JsonInput): Try[StateInfo] = input.deserialize[StateInfo]
}
