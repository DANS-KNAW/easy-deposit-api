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
import java.net.URL
import java.nio.file.{ NoSuchFileException, Paths }
import java.util.UUID

import better.files._
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.Errors._
import nl.knaw.dans.easy.deposit.PidRequesterComponent.{ PidRequester, PidType }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.easy.deposit.docs._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC

import scala.collection.Seq
import scala.util.{ Failure, Success, Try }

/**
 * Represents an existing deposit directory.
 *
 * @param draftBase the base directory for the deposits
 * @param user      the user ID of the deposit's owner
 * @param id        the ID of the deposit
 */
case class DepositDir private(draftBase: File, user: String, id: UUID) extends DebugEnhancedLogging {
  val bagDir: File = draftBase / user / id.toString / bagDirName
  private val metadataDir = bagDir / "metadata"
  private val depositPropertiesFile = bagDir.parent / "deposit.properties"
  private val datasetMetadataJsonFile = metadataDir / "dataset.json"

  /**
   * @param submitBase the base directory with submitted deposits to extract the actual state
   * @return the `StateManager` for this deposit
   */
  def getStateManager(submitBase: File, easyHome: URL): StateManager = {
    StateManager(bagDir.parent, submitBase, easyHome)
  }

  /**
   * @param submitBase the base directory with submitted deposits to extract the actual state
   * @return basic information about the deposit.
   */
  def getDepositInfo(submitBase: File, easyHome: URL): Try[DepositInfo] = {
    for {
      title <- getDatasetTitle
      stateManager = getStateManager(submitBase, easyHome)
      stateInfo <- stateManager.getStateInfo
      created = new DateTime(stateManager.draftProps.getString("creation.timestamp")).withZone(UTC)
    } yield DepositInfo(
      id,
      title,
      stateInfo.state,
      stateInfo.stateDescription,
      created
    )
  }.recoverWith {
    case t: CorruptDepositException => Failure(t)
    case t => corruptDepositFailure(t)
  }

  private def getDatasetTitle: Try[String] = {
    getDatasetMetadata
      .map(_.titles.flatMap(_.headOption).getOrElse(""))
  }

  private def getDepositProps: Try[PropertiesConfiguration] = Try {
    new PropertiesConfiguration(depositPropertiesFile.toJava)
  }.flatMap {
    case props if props.getKeys.hasNext => Success(props)
    case _ => Failure(CorruptDepositException(user, id.toString, new Exception("deposit.properties not found or empty")))
  }

  /**
   * @return the dataset level metadata in this deposit
   */
  def getDatasetMetadata: Try[DatasetMetadata] = {
    Try { datasetMetadataJsonFile.fileInputStream }
      .flatMap(_ (is => DatasetMetadata(is)))
      .recoverWith {
        case t: InvalidDocumentException => corruptDepositFailure(t)
        case t: FileNotFoundException => corruptDepositFailure(t)
        case t: NoSuchFileException => corruptDepositFailure(t)
        case t => Failure(t)
      }
  }

  private def corruptDepositFailure(t: Throwable) = {
    Failure(CorruptDepositException(user, id.toString, t))
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
  }

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
    _ <- dm.doi.map(_ => doisMatch(dm, maybeDOI)).getOrElse(Success(()))
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
      logger.error(s"DOI in datasetmetadata.json [${ dm.doi }] does not equal DOI in deposit.properties [$doi]")
      Failure(InvalidDoiException(id))
    }
  }
}

object DepositDir {

  /**
   * Lists the deposits of the specified user.
   *
   * @param draftDir the base directory for all draft deposits.
   * @param user     the user name
   * @return a list of [[DepositDir]] objects
   */
  def list(draftDir: File, user: String): Seq[DepositDir] = {
    val userDir = draftDir / user
    if (userDir.exists)
      userDir
        .list
        .withFilter(_.isDirectory)
        .map(deposit => Try {
          DepositDir(draftDir, user, UUID.fromString(deposit.name))
        }.recoverWith { case t: Throwable => Failure(CorruptDepositException(user, deposit.name, t)) })
        .collect { case Success(deposit: DepositDir) => deposit }
        .toSeq
    else Seq.empty
  }

  /**
   * Returns the requested [[DepositDir]], if it is owned by `user`
   *
   * @param draftDir the base directory for all draft deposits
   * @param user     the user name
   * @param id       the identifier of the deposit
   * @return a [[DepositDir]] object
   */
  def get(draftDir: File, user: String, id: UUID): Try[DepositDir] = {
    val depositDir = DepositDir(draftDir, user, id)
    if (depositDir.bagDir.parent.exists) Success(depositDir)
    else Failure(NoSuchDepositException(user, id, new FileNotFoundException()))
  }

  /**
   * Creates and returns a new deposit for `user`.
   *
   * @param draftDir the base directory for all draft deposits
   * @param user     the user name
   * @return the newly created [[DepositDir]]
   */
  def create(draftDir: File, user: String): Try[DepositDir] = {
    val depositInfo = DepositInfo()
    val deposit = DepositDir(draftDir, user, depositInfo.id)
    val depositDir = deposit.draftBase / user / depositInfo.id.toString
    for {
      _ <- Try { depositDir.createDirectories }
      bag <- DansV0Bag.empty(depositDir / bagDirName)
      _ = bag.withEasyUserAccount(deposit.user)
      _ <- bag.addTagFile("{}".inputStream, Paths.get("metadata/dataset.json"))
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
      addProperty("identifier.dans-doi.registered", "no")
      addProperty("identifier.dans-doi.action", "create")
      addProperty("bag-store.bag-name", bagDirName)
    }.save(depositDir.depositPropertiesFile.toJava)
  }
}
