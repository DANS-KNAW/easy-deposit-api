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
    (submitBase / getProp(bagIdKey) / "deposit.properties").toJava
  )

  def getStateInfo: Try[StateInfo] = Try {
    val draftState = getStateLabel()
    draftState match {
      case State.submitted | State.inProgress =>
        val newState: State = getProp(stateLabelKey, submittedProps) match {
          case "REJECTED" => State.rejected
          case "FEDORA_ARCHIVED" => State.archived
          case "SUBMITTED" => State.submitted
          case "IN_REVIEW" | "FAILED" => State.inProgress
          case str: String => throw InvalidPropertyException(stateLabelKey, str, submittedProps)
        }
        saveNewState(StateInfo(newState, getStateDescription(submittedProps)))
      case State.draft | State.rejected | State.archived =>
        StateInfo(draftState, getStateDescription())
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

  /** @return bag-store.bag-id in case the new state is submitted */
  def changeState(newStateInfo: StateInfo): Try[UUID] = getStateInfo.flatMap { old =>
    // getStateInfo has been called by canChangeState, but it is not an IO action so no optimisation
    (old.state, newStateInfo.state) match {
      case (State.draft, State.submitted) =>
        val bagStoreBagId = UUID.randomUUID()
        draftProps.setProperty(bagIdKey, bagStoreBagId)
        saveNewState(newStateInfo)
        Success(bagStoreBagId)
      case (State.rejected, State.draft) =>
        draftProps.clearProperty(bagIdKey)
        saveNewState(newStateInfo)
        Success(null)
      case (oldState, newState) =>
        Failure(IllegalStateTransitionException(oldState, newState))
    }
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

  private def getStateDescription(props: PropertiesConfiguration = draftProps): String = {
    getProp(stateDescriptionKey, props)
  }

  private def getProp(key: String, props: PropertiesConfiguration = draftProps): String = {
    Option(props.getString(key))
      .getOrElse(throw PropertyNotFoundException(key, props))
  }
}
