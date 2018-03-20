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

import better.files._
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.format.{ DateTimeFormatter, ISODateTimeFormat }
import org.joda.time.{ DateTime, DateTimeZone }

import scala.util.Try

/**
 * Represents an existing deposit directory.
 *
 * @param baseDir the base directory for the deposits
 * @param user    the user ID of the deposit's owner
 * @param id      the ID of the deposit
 */
case class DepositDir private(baseDir: File, user: String, id: UUID) {

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
  def getDepositInfo: Try[DepositInfo] = ???

  /**
   * @return the dataset level metadata in this deposit
   */
  def getDatasetMetadata: Try[DatasetMetadata] = ???

  /**
   * Writes the dataset level metadata for this deposit.
   *
   * @param md the metadata to write
   */
  def setDatasetMetadata(md: DatasetMetadata): Try[Unit] = ???

  /**
   * @return object to access the data files of this deposit
   */
  def getDataFiles: Try[DataFiles] = Try {
    new DataFiles(baseDir / user / id.toString / "data", baseDir / user / id.toString / "data" / "metadata" / "files.xml")
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
  def list(baseDir: File, user: String): Try[Seq[DepositDir]] = ???

  /**
   * Returns the requested [[DepositDir]], if it is owned by `user`
   *
   * @param baseDir the base directory for all draft deposits
   * @param user    the user name
   * @param id      the identifier of the deposit
   * @return a [[DepositDir]] object
   */
  def get(baseDir: File, user: String, id: UUID): Try[DepositDir] = ???

  /**
   * Creates and returns a new deposit for `user`.
   *
   * @param baseDir the base directory for all draft deposits
   * @param user    the user name
   * @return the newly created [[DepositDir]]
   */
  def create(baseDir: File, user: String): Try[DepositDir] = Try {

    val uuid = UUID.randomUUID()
    val dir: File = (baseDir / user / uuid.toString)
      .createIfNotExists(asDirectory = true, createParents = true)

    val dateTimeFormatter: DateTimeFormatter = ISODateTimeFormat.dateTime()
    val props = new PropertiesConfiguration()
    props.addProperty("creation.timestamp", DateTime.now(DateTimeZone.UTC).toString(dateTimeFormatter))
    props.addProperty("state.label", "DRAFT")
    props.addProperty("state.description", "some test description") // TODO what's the real value here?
    props.addProperty("depositor.userId", user)
    props.save((dir / "deposit.properties").toJava)

    DepositDir(baseDir, user, uuid)
  }
}



