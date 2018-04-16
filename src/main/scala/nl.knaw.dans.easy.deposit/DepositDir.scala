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
import nl.knaw.dans.easy.deposit.docs.Json.{ InvalidDocumentException, toJson }
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, Json }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC
import org.json4s.StreamInput

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

  private val dataDir = baseDir / user / id.toString / "bag"
  private val metadataDir = dataDir / "metadata"

  /**
   * @return an information object about the current state of the desposit.
   */
  def getStateInfo: Try[StateInfo] = ???

  /**
   * Sets changes the state of the deposit. If the state transition is not allow a `Failure` containing
   * an [[IllegalStateTransitionException]] is returned.
   *
   * @param stateInfo the new state
   * @return success of failure
   */
  def setStateInfo(stateInfo: StateInfo): Try[Unit] = ???

  /**
   * Deletes the deposit.
   */
  def delete(): Try[Unit] = ???

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
        case t: NoSuchDepositException => Success("")
        case t => Failure(t)
      }
  }

  private def getDepositProps = {
    val props = new PropertiesConfiguration()
    Try { (dataDir.parent / "deposit.properties").fileReader }
      .flatMap(_ (is => Try { props.load(is) }))
      .map(_ => props)
  }

  /**
   * @return the dataset level metadata in this deposit
   */
  def getDatasetMetadata: Try[DatasetMetadata] = {
    Try { (metadataDir / "dataset.json").fileInputStream }
      .flatMap(_ (is => Json.getDatasetMetadata(StreamInput(is))))
      .recoverWith {
        case t: InvalidDocumentException => Failure(CorruptDepositException(user, id.toString, t))
        case _: FileNotFoundException => notFoundFailure()
        case _: NoSuchFileException => notFoundFailure()
        case t => Failure(t)
      }
  }

  private def notFoundFailure() = {
    Failure(NoSuchDepositException(
      user,
      id,
      new Exception(s"File not found: $metadataDir/dataset.json")
    ))
  }

  /**
   * Writes the dataset level metadata for this deposit.
   *
   * @param md the metadata to write
   */
  def setDatasetMetadata(md: DatasetMetadata): Try[Unit] = Try {
    // TODO EasyDepositApiApp.writeDataMetadataToDeposit says: should be complete
    // TODO Who is responsible? I suppose also DOI should not change.
    (metadataDir / "dataset.json").write(toJson(md))
    () // satisfy the compiler which doesn't want a File
  }.recoverWith { case _: NoSuchFileException => notFoundFailure() }

  /**
   * @return object to access the data files of this deposit
   */
  def getDataFiles: Try[DataFiles] = Try {
    new DataFiles(dataDir, metadataDir / "files.xml")
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
  def get(baseDir: File, user: String, id: UUID): Try[DepositDir] = Try {
    DepositDir(baseDir, user, id)
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
    val depositDir: File = (baseDir / user / depositInfo.id.toString)
      .createIfNotExists(asDirectory = true, createParents = true)

    val metadata = new BagitMetadata {
      add("Created", depositInfo.timestampString)
      add("Bag-Size", "0 KB")
    }
    val bagDir: File = depositDir / "bag"
    bagDir.createDirectory()
    BagCreator.bagInPlace(bagDir.path, JArrays.asList(StandardSupportedAlgorithms.SHA1), true, metadata)
    (bagDir / "metadata").createIfNotExists(asDirectory = true)

    new PropertiesConfiguration() {
      addProperty("creation.timestamp", depositInfo.timestamp)
      addProperty("state.label", depositInfo.state.toString)
      addProperty("state.description", depositInfo.stateDescription)
      addProperty("depositor.userId", user)
    }.save((depositDir / "deposit.properties").toJava)

    DepositDir(baseDir, user, depositInfo.id)
  }
}



