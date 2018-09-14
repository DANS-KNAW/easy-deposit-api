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

import java.io.InputStream
import java.net.URI
import java.nio.file.{ Path, Paths }
import java.util.UUID

import better.files.File.temporaryDirectory
import better.files.{ File, ManagedResource }
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.authentication.LdapAuthentication
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, StateInfo }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.Try

class EasyDepositApiApp(configuration: Configuration) extends DebugEnhancedLogging
  with LdapAuthentication
  with PidRequesterComponent {

  val properties: PropertiesConfiguration = configuration.properties

  override val pidRequester: PidRequester = new PidRequester {
    override val pidGeneratorService: URI = new URI(properties.getString("pids.generator-service"))
    logger.info(s"pids.generator-service = $pidGeneratorService")
  }
  override val authentication: Authentication = new Authentication {
    override val ldapUserIdAttrName: String = properties.getString("users.ldap-user-id-attr-name")
    override val ldapParentEntry: String = properties.getString("users.ldap-parent-entry")
    override val ldapProviderUrl: String = properties.getString("users.ldap-url")
    override val ldapAdminPrincipal: String = properties.getString("users.ldap-admin-principal")
    override val ldapAdminPassword: String = properties.getString("users.ldap-admin-password")
    logger.info(s"users.ldap-url = $ldapProviderUrl")
    logger.info(s"users.ldap-parent-entry = $ldapParentEntry")
    logger.info(s"users.ldap-user-id-attr-name = $ldapUserIdAttrName")
    logger.info(s"users.ldap-admin-principal = $ldapAdminPrincipal")
    logger.info(s"users.ldap-admin-password = $ldapAdminPassword") // TODO configured in same security context as logged?
  }

  def getVersion: String = {
    configuration.version
  }

  private val uploadStagingDir = getConfiguredDirectory("deposits.stage.upload")
  private val draftsDir = getConfiguredDirectory("deposits.drafts")
  private val submitter = new Submitter(
    stagingBaseDir = getConfiguredDirectory("deposits.stage"),
    submitToBaseDir = getConfiguredDirectory("deposits.submit-to"),
    pidRequester
  )

  private def getConfiguredDirectory(key: String): File = {
    val dir = File(configuration.properties.getString(key))

    if (!dir.exists) throw ConfigurationException(s"Configured directory '$key' does not exist: $dir")
    if (!dir.isDirectory) throw ConfigurationException(s"Configured directory '$key' is a regular file: $dir")
    if (!dir.isReadable) throw ConfigurationException(s"Configured directory '$key' is not readable: $dir")

    dir
  }

  def getUser(user: String): Try[Map[String, Seq[String]]] = authentication.getUser(user)

  /**
   * Creates a new, empty deposit, containing an empty bag in the user's draft area. If the user
   * has no draft area yet, it is first created.
   *
   * @param user the user ID
   * @return the new deposit's ID
   */
  def createDeposit(user: String): Try[DepositInfo] = {
    DepositDir.create(draftsDir, user).flatMap(_.getDepositInfo)
  }

  /**
   * Returns the list of `DepositInfo` objects for the deposits in the user's draft area.
   *
   * @param user the user ID
   * @return a list of [[docs.DepositInfo]] objects
   */
  def getDeposits(user: String): Try[Seq[DepositInfo]] = {
    for {
      deposits <- DepositDir.list(draftsDir, user)
      infos <- deposits.map(_.getDepositInfo).collectResults
    }
      yield infos
  }

  /**
   * Returns the current state of the deposit.
   *
   * @param user ID
   * @param id   the deposit ID
   * @return
   */
  def getDepositState(user: String, id: UUID): Try[StateInfo] = {
    for {
      deposit <- DepositDir.get(draftsDir, user, id)
      state <- deposit.getStateInfo
    } yield state
  }

  /**
   * Sets the deposit state. The only legal transitions are:
   *
   * - from [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.draft]] to [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.submitted]]
   * - from [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.rejected]] to [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.draft]]
   *
   * Any attempt at another transition will result in an [[nl.knaw.dans.easy.deposit.IllegalStateTransitionException]].
   *
   * When transitioning to [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.submitted]] the following steps will be executed:
   *
   * 1. The `files.xml` of the bag will be written.
   * 2. The SHA-1 payload manifest will be calculated and written.
   * 3. The deposit directory will be copied to the staging area.
   * 4. The copy will be moved to the deposit area.
   *
   * @param user      the user ID
   * @param id        the deposit ID
   * @param stateInfo the state to transition to
   * @return
   */
  def setDepositState(stateInfo: StateInfo, user: String, id: UUID): Try[Unit] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    _ <- deposit.checkStateTransition(stateInfo.state)
    _ <- if (stateInfo.state == State.submitted)
           submitter.submit(deposit) // also changes the state
         else deposit.setStateInfo(stateInfo)
  } yield ()

  /**
   * Deletes the deposit from the user's draft area.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @return
   */
  def deleteDeposit(user: String, id: UUID): Try[Unit] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    _ <- deposit.delete()
  } yield ()

  /**
   * Returns the DOI as stored `dataset.xml` respective `deposit.properties`.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @return
   */
  def getDoi(user: String, id: UUID): Try[String] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    doi <- deposit.getDOI(pidRequester)
  } yield doi

  /**
   * Returns the dataset metadata from `dataset.xml`.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @return
   */
  def getDatasetMetadataForDeposit(user: String, id: UUID): Try[DatasetMetadata] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    md <- deposit.getDatasetMetadata
  } yield md

  /**
   * Writes the provided [[docs.DatasetMetadata]] object as `dataset.xml` to the deposit directory. Any
   * existing `dataset.xml` is overwritten, so it is important that the object contains the complete
   * current metadata.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @param dm   the dataset metadata
   * @return
   */
  def writeDataMetadataToDeposit(dm: DatasetMetadata, user: String, id: UUID): Try[Unit] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    _ <- deposit.writeDatasetMetadataJson(dm)
  } yield ()

  /**
   * Returns the list of [[FileInfo]] objects for the dataset files in this deposit.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @return
   */
  def getDepositFiles(user: String, id: UUID, path: Path = Paths.get("")): Try[Seq[FileInfo]] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    dataFiles <- deposit.getDataFiles
    fileInfos <- dataFiles.list(path)
  } yield fileInfos

  /**
   * Writes the given input stream to a location in the deposit's content directory. The specified `path`
   * must be relative to the content directory. The function first ensures that the parent directory
   * exists and creates it, if it doesn't.
   *
   * If a new file is created the function returns `true`, otherwise an existing file was overwritten.
   * (We hope the reader will forgive us this minor violation of the Command-Query separation principle.)
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @param path the path of the file to (over)write, relative to the content base directory
   * @param is   the input stream to write from
   * @return `true` if a new file was created, `false` otherwise
   */
  def writeDepositFile(is: => InputStream, user: String, id: UUID, path: Path): Try[Boolean] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    dataFiles <- deposit.getDataFiles
    _ = logger.info(s"uploading to [${ dataFiles.bag.baseDir }] of [$path]")
    created <- dataFiles.write(is, path)
    _ = logger.info(s"created=$created $user/$id/$path")
  } yield created

  def stageFiles(userId: String, id: UUID, destination: Path): Try[(ManagedResource[File], StagedFilesTarget)] = for {
    deposit <- DepositDir.get(draftsDir, userId, id)
    dataFiles <- deposit.getDataFiles
    stagingDir = temporaryDirectory(s"$userId-$id-", Some(uploadStagingDir.createDirectories()))
  } yield (stagingDir, StagedFilesTarget(dataFiles.bag, destination))

  /**
   * Deletes the given regular file or directory. The specified `path` must be relative to the content directory.
   * If the file is a directory, it is deleted including all the files and directories in it.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @param path the path of the file to delete, relative to the content base directory
   * @return
   */
  def deleteDepositFile(user: String, id: UUID, path: Path): Try[Unit] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    dataFiles <- deposit.getDataFiles
    _ <- dataFiles.delete(path)
  } yield ()
}
