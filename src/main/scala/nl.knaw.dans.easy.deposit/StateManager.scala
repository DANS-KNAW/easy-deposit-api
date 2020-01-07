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
import org.apache.commons.configuration.{ ConfigurationException, PropertiesConfiguration }
import org.scalatra.util.UrlCodingUtils.queryPartEncode

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
  private lazy val submittedProps = getProp(bagIdKey, draftProps)
    .map(submittedId => {
      val submitDepositPropsFile = submitBase / submittedId / "deposit.properties"
      if (submitDepositPropsFile.exists)
        new PropertiesConfiguration(submitDepositPropsFile.toJava)
      else
        draftProps
    })
    .getOrElse(new PropertiesConfiguration) // not expected to happen, since `bagIdKey` should always exist in `draftProps`

  /** @return the state-label/description from drafts/USER/UUID/deposit.properties
   *          unless more recent values might be available in SUBMITTED/UUID/deposit.properties
   */
  def getStateInfo: Try[StateInfo] = getStateInDraftDeposit.flatMap {
    case draftState @ (State.draft | State.rejected | State.archived) =>
      Success(StateInfo(draftState, getStateDescription(draftProps)))
    case stateInDraftDeposit @ (State.submitted | State.inProgress) =>
      getProp(stateLabelKey, submittedProps).map {
        case "SUBMITTED" => StateInfo(stateInDraftDeposit, getStateDescription(draftProps))
        case "REJECTED" => getProp("curation.performed", submittedProps) match {
          case Success("yes") => saveNewStateInDraftDeposit(StateInfo(State.rejected, getStateDescription(submittedProps)))
          case _ => StateInfo(stateInDraftDeposit, mailToDansMessage)
        }
        case "FAILED" => StateInfo(stateInDraftDeposit, mailToDansMessage)
        case "IN_REVIEW" => saveNewStateInDraftDeposit(StateInfo(State.inProgress, landingPage("The deposit is available at")))
        case "FEDORA_ARCHIVED" |
             "ARCHIVED" => saveNewStateInDraftDeposit(StateInfo(State.archived, landingPage("The dataset is published at")))
        case str =>
          logger.error(InvalidPropertyException(stateLabelKey, str, submittedProps).getMessage)
          StateInfo(stateInDraftDeposit, mailToDansMessage)
      }.recoverWith { case e =>
        logger.error(s"Could not find state of submitted deposit [draft = $relativeDraftDir]: ${ e.getMessage }")
        Success(StateInfo(stateInDraftDeposit, mailToDansMessage))
      }
  }

  def setStateFailed(description: String): Try[Unit] = Try {
    saveNewStateInDraftDeposit(StateInfo(State.submitted, description))
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
    logger.info(s"[${ draftDeposit.id }] changing deposit state from $oldStateInfo to $newStateInfo")

    // getStateInfo has been called by canChangeState, but it is not an IO action
    // so let's keep it simple without optimisation
    (oldStateInfo.state, newStateInfo.state) match {
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

  /**
   * @return a message that overrides the status message of a technical problem
   *         in easy-ingest-flow or with the preparation of its input
   */
  private def mailToDansMessage: String = {
    val title: String = (draftDeposit.getDatasetMetadata.map(_.titles) match {
      case Success(Some(titles)) => titles.headOption
      case _ => None
    }).getOrElse("").trim()
    val shortTitle = title.substring(0, Math.min(42, title.length))
      .replaceAll("[\r\n]+", " ") // wrap a multiline title into a single line
      .replaceAll("<.*", "") // avoid html tags in subject and body
    val ellipsis = if (shortTitle == title) ""
                   else "…"
    val ref = getSubmittedBagId.map(_.toString).getOrElse(s"DRAFT/$relativeDraftDir")
    val subject = queryPartEncode(s"Deposit processing error: $shortTitle$ellipsis reference $ref")
    val body = queryPartEncode(
      s"""Dear data manager,
         |
         |Something went wrong while processing my deposit. Could you please investigate the issue?
         |
         |Dataset reference:
         |   $ref
         |Title:
         |   $shortTitle$ellipsis
         |
         |Kind regards,
         |${ draftDeposit.user }
         |""".stripMargin
    )
    s"""Something went wrong while processing this deposit. Please <a href="mailto:info@dans.knaw.nl?subject=$subject&body=$body">contact DANS</a>"""
  }

  private def landingPage(msgStart: String): String = {
    val url = getProp("identifier.fedora", submittedProps)
      .map(id => s"$easyHome/datasets/id/$id")
      .getOrElse(s"$easyHome/mydatasets") // fall back
    s"""$msgStart <a href="$url" target="_blank">$url</a>"""
  }

  private def saveNewStateInDraftDeposit(newStateInfo: StateInfo): StateInfo = {
    draftProps.setProperty(stateLabelKey, newStateInfo.state.toString)
    draftProps.setProperty(stateDescriptionKey, newStateInfo.stateDescription)
    draftProps.save()
    newStateInfo
  }

  private def getStateInDraftDeposit: Try[State.Value] = {
    getProp(stateLabelKey, draftProps).map(State.withName)
  }

  private def getStateDescription(props: PropertiesConfiguration, default: String = ""): String = {
    getProp(stateDescriptionKey, props).getOrElse(default)
  }

  private def getProp(key: String, props: PropertiesConfiguration): Try[String] = Try {
    Option(props.getString(key, null))
      .getOrElse(throw PropertyNotFoundException(key, props))
  }
}
