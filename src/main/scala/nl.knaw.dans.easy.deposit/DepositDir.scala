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
import java.nio.file.NoSuchFileException
import java.util.{ UUID, Arrays => JArrays }

import better.files._
import gov.loc.repository.bagit.creator.BagCreator
import gov.loc.repository.bagit.domain.{ Metadata => BagitMetadata }
import gov.loc.repository.bagit.hash.StandardSupportedAlgorithms
import nl.knaw.dans.easy.deposit.PidRequesterComponent.{ PidRequester, PidType }
import nl.knaw.dans.easy.deposit.State.State
import nl.knaw.dans.easy.deposit.docs.Json.{ InvalidDocumentException, toJson }
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, Json }
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
  private val dataFilesDir = bagDir / "data"
  private val depositPropertiesFile = bagDir.parent / "deposit.properties"
  private val datasetMetadataFile = metadataDir / "dataset.json"

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
      currentState = State.withName(props.getString("state.label"))
      _ <- checkStateTransition(currentState, stateInfo.state)
      _ = props.setProperty("state.label", stateInfo.state.toString)
      _ = props.setProperty("state.description", stateInfo.stateDescription.toString)
      _ = props.save()
    } yield ()
  }

  private def checkStateTransition(transition: (State, State)) = {
    transition match {
      case (State.DRAFT, State.SUBMITTED) => Success(())
      case (State.REJECTED, State.DRAFT) => Success(())
      case (oldState, newState) => Failure(IllegalStateTransitionException(user, id, oldState, newState))
    }
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
    Try { datasetMetadataFile.fileInputStream }
      .flatMap(_ (is => Json.getDatasetMetadata(is)))
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
   */
  def setDatasetMetadata(md: DatasetMetadata): Try[Unit] = Try {
    // TODO DOI should not change, only enforced by client calling getDOI. State and dateSubmitted should not change either.
    datasetMetadataFile.write(toJson(md))
    () // satisfy the compiler which doesn't want a File
  }.recoverWith { case _: NoSuchFileException => notFoundFailure() }

  /** part of submit sequence */
  def writeSplittedDatasetMetadata: Try[Unit] = {
    for {
      oldDatasetMetadata <- getDatasetMetadata
      datasetMetadata <- oldDatasetMetadata.setDateSubmitted()
      _ <- setDatasetMetadata(datasetMetadata)
      _ = (metadataDir / "message-from-depositor.txt").write(datasetMetadata.messageForDataManager.getOrElse(""))
      agreements <- datasetMetadata.agreements(user)
      _ <- agreements.writePretty(metadataDir / "agreements.xml")
      datasetMetadataXML <- datasetMetadata.xml
      _ <- datasetMetadataXML.writePretty(metadataDir / "dataset.xml")
    } yield ()
  }

  /**
   * @return object to access the data files of this deposit
   */
  def getDataFiles: Try[DataFiles] = Try {
    DataFiles(dataFilesDir, metadataDir / "files.xml")
  }

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
    _ <- maybeTriedDOI.getOrElse(setDatasetMetadata(dm.copy(doi = Some(doi))))
  } yield doi

  private def doisMatch(dm: DatasetMetadata, doi: Option[String]) = {
    if (doi == dm.doi) Success(())
    else {
      val e = new Exception(s"DOI in dataset.xml [${ dm.doi }] does not equal DOI in deposit.properties [$doi]")
      Failure(CorruptDepositException(user, id.toString, e))
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
  def create(baseDir: File, user: String): Try[DepositDir] = Try {
    val depositInfo = DepositInfo()
    val depositDir = DepositDir(baseDir, user, depositInfo.id)
    depositDir.bagDir.createIfNotExists(asDirectory = true, createParents = true)

    val bagitdMetadata = new BagitMetadata {
      add("Created", depositInfo.date.toString)
      add("Bag-Size", "0 KB")
    }
    BagCreator.bagInPlace(depositDir.bagDir.path, JArrays.asList(StandardSupportedAlgorithms.SHA1), true, bagitdMetadata)

    // creating the following before the bag would move the metadata directory into bag/data
    depositDir.metadataDir.createIfNotExists(asDirectory = true)
    depositDir.datasetMetadataFile.write("{}")

    new PropertiesConfiguration() {
      addProperty("creation.timestamp", depositInfo.date)
      addProperty("state.label", depositInfo.state.toString)
      addProperty("state.description", depositInfo.stateDescription)
      addProperty("depositor.userId", user)
    }.save(depositDir.depositPropertiesFile.toJava)

    depositDir
  }
}



