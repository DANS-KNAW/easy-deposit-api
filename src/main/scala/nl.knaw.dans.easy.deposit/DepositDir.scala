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

import java.io.FileNotFoundException
import java.nio.file.{ NoSuchFileException, Paths }
import java.util.UUID

import better.files._
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.PidRequesterComponent.{ PidRequester, PidType }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, toJson }
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import nl.knaw.dans.easy.deposit.docs.{ StateInfo, _ }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC

import scala.collection.Seq
import scala.util.{ Failure, Success, Try }

/**
 * Represents an existing deposit directory.
 *
 * @param baseDir the base directory for the deposits
 * @param user    the user ID of the deposit's owner
 * @param id      the ID of the deposit
 */
case class DepositDir private(baseDir: File, user: String, id: UUID) extends DebugEnhancedLogging {
  private val bagDir = baseDir / user / id.toString / "bag"
  private val metadataDir = bagDir / "metadata"
  private val depositPropertiesFile = bagDir.parent / "deposit.properties"
  private val datasetMetadataJsonFile = metadataDir / "dataset.json"

  /**
   * @return an information object about the current state of the desposit.
   */
  def getStateInfo: Try[StateInfo] = {
    for {
      props <- getDepositProps
      state = State.withName(props.getString("state.label"))
      description = props.getString("state.description")
    } yield StateInfo(state, description)
  }

  /**
   * Sets changes the state of the deposit. If the state transition is not allow a `Failure` containing
   * an [[IllegalStateTransitionException]] is returned.
   *
   * @param stateInfo the new state
   * @return success of failure
   */
  def setStateInfo(stateInfo: StateInfo): Try[Unit] = {
    for {
      props <- getDepositProps
      _ <- checkStateChange(stateInfo.state, props)
      _ = props.setProperty("state.label", stateInfo.state.toString)
      _ = props.setProperty("state.description", stateInfo.stateDescription.toString)
      _ = props.save()
    } yield ()
  }

  def checkStateTransition(newState: State): Try[PropertiesConfiguration] = {
    for {
      props <- getDepositProps
      _ <- checkStateChange(newState, props)
    } yield props
  }

  private def checkStateChange(newState: State, props: PropertiesConfiguration) = {
    for {
      currentState <- Try { State.withName(props.getString("state.label")) }
      _ <- (currentState, newState) match {
        case (State.draft, State.submitted) => Success(())
        case (State.rejected, State.draft) => Success(())
        case _ => Failure(IllegalStateTransitionException(user, id, currentState, newState))
      }
    } yield ()
  }

  /**
   * Deletes the deposit.
   */
  def delete(): Try[Unit] = Try {
    bagDir.parent.delete()
  }

  /**
   * @return basic information about the deposit.
   */
  def getDepositInfo: Try[DepositInfo] = {
    for {
      title <- getDatasetTitle
      props <- getDepositProps
      state = State.withName(props.getString("state.label"))
      created = new DateTime(props.getString("creation.timestamp")).withZone(UTC)
    } yield DepositInfo(
      id,
      title,
      state,
      props.getString("state.description"),
      created
    )
  }.recoverWith {
    case t: CorruptDepositException => Failure(t)
    case _: FileNotFoundException => notFoundFailure()
    case _: NoSuchFileException => notFoundFailure()
    case t => Failure(CorruptDepositException(user, id.toString, t))
  }

  private def getDatasetTitle = {
    getDatasetMetadata
      .map(_.titles.flatMap(_.headOption).getOrElse(""))
      .recoverWith {
        case _: NoSuchDepositException => Success("")
        case t => Failure(t)
      }
  }

  private def getDepositProps = Try {
    new PropertiesConfiguration(depositPropertiesFile.toJava)
  }

  /**
   * @return the dataset level metadata in this deposit
   */
  def getDatasetMetadata: Try[DatasetMetadata] = {
    Try { datasetMetadataJsonFile.fileInputStream }
      .flatMap(_ (is => DatasetMetadata(is)))
      .recoverWith {
        case t: InvalidDocumentException => Failure(CorruptDepositException(user, id.toString, t))
        case _: FileNotFoundException => notFoundFailure()
        case _: NoSuchFileException => notFoundFailure()
        case t => Failure(t)
      }
  }

  private def notFoundFailure() = {
    Failure(NoSuchDepositException(user, id, new Exception(s"File not found: $metadataDir/dataset.json")))
  }

  /**
   * Writes the dataset level metadata for this deposit.
   *
   * @param md the metadata to write
   *           In terms of JSon syntax, content like
   *           "identifiers": [{ "scheme": "id-type:DOI", "value": "..."}]
   *           should have been acquired with an explicit getDOI request.
   *           Otherwise submit will fail because the value
   *           is out of sync with deposit properties.
   *           JSon content like
   *           "dates": [ { ..., "qualifier": "dcterms:dateSubmitted" }]
   *           will cause an error when converted to dataset.xml at submit.
   */
  def writeDatasetMetadataJson(md: DatasetMetadata): Try[Unit] = Try {
    datasetMetadataJsonFile.write(toJson(md))
    () // satisfy the compiler which doesn't want a File
  }.recoverWith { case _: NoSuchFileException => notFoundFailure() }

  /**
   * @return object to access the data files of this deposit
   */
  def getDataFiles: Try[DataFiles] = DansV0Bag.read(bagDir).map(DataFiles)

  /**
   * @param pidRequester used to mint a new doi if none was found yet
   * @return the doi as stored in deposit.properties and dataset.xml
   */
  def getDOI(pidRequester: PidRequester): Try[String] = for {
    dm <- getDatasetMetadata
    props <- getDepositProps
    maybeDOI = Option(props.getString("identifier.doi", null))
    _ <- doisMatch(dm, maybeDOI)
    maybeTriedDOI = maybeDOI.map(Success(_))
    doi <- maybeTriedDOI.getOrElse(pidRequester.requestPid(PidType.doi))
    _ = props.addProperty("identifier.doi", doi)
    _ <- maybeTriedDOI.getOrElse(Try { props.save(depositPropertiesFile.toJava) })
    _ <- maybeTriedDOI.getOrElse(writeDatasetMetadataJson(dm.setDoi(doi)))
  } yield doi

  def sameDOIs(dm: DatasetMetadata): Try[Unit] = for {
    props <- getDepositProps
    maybeDOI = Option(props.getString("identifier.doi", null))
    _ <- doisMatch(dm, maybeDOI)
  } yield ()

  private def doisMatch(dm: DatasetMetadata, doi: Option[String]) = {
    if (doi == dm.doi) Success(())
    else {
      Failure(BadDoiException(dm, doi))
    }
  }
}

object DepositDir {

  /**
   * Lists the deposits of the specified user.
   *
   * @param baseDir the base directory for all draft deposits.
   * @param user    the user name
   * @return a list of [[DepositDir]] objects
   */
  def list(baseDir: File, user: String): Try[Seq[DepositDir]] = {
    val userDir = baseDir / user
    if (userDir.exists)
      userDir
        .list
        .filter(_.isDirectory)
        .map(deposit => Try {
          new DepositDir(baseDir, user, UUID.fromString(deposit.name))
        }.recoverWith { case t: Throwable => Failure(CorruptDepositException(user, deposit.name, t)) })
        .toSeq
        .collectResults
    else Try { Seq.empty }
  }

  /**
   * Returns the requested [[DepositDir]], if it is owned by `user`
   *
   * @param baseDir the base directory for all draft deposits
   * @param user    the user name
   * @param id      the identifier of the deposit
   * @return a [[DepositDir]] object
   */
  def get(baseDir: File, user: String, id: UUID): Try[DepositDir] = {
    val depositDir = DepositDir(baseDir, user, id)
    if (depositDir.baseDir.exists) Success(depositDir)
    else Failure(NoSuchDepositException(user, id, null))
  }

  /**
   * Creates and returns a new deposit for `user`.
   *
   * @param baseDir the base directory for all draft deposits
   * @param user    the user name
   * @return the newly created [[DepositDir]]
   */
  def create(baseDir: File, user: String): Try[DepositDir] = {
    val depositInfo = DepositInfo()
    val deposit = DepositDir(baseDir, user, depositInfo.id)
    val depositDir = deposit.baseDir / user / depositInfo.id.toString
    for {
      _ <- Try { depositDir.createDirectories }
      bag <- DansV0Bag.empty(depositDir / "bag")
      _ = bag.withEasyUserAccount(deposit.user)
      _ <- bag.addTagFile("{}".asInputStream, Paths.get("metadata/dataset.json"))
      _ <- bag.save()
      _ <- createDepositProperties(user, depositInfo, deposit)
    } yield deposit
  }

  private def createDepositProperties(user: String, depositInfo: DepositInfo, depositDir: DepositDir) = Try {
    new PropertiesConfiguration() {
      addProperty("creation.timestamp", depositInfo.date)
      addProperty("state.label", depositInfo.state.toString)
      addProperty("state.description", depositInfo.stateDescription)
      addProperty("depositor.userId", user)
      addProperty("curation.required", "yes")
      addProperty("curation.performed", "no")
      addProperty("bag-store.bag-id", depositInfo.id)
    }.save(depositDir.depositPropertiesFile.toJava)
  }
}
