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

import java.io.{ IOException, InputStream }
import java.net.URI
import java.nio.file.attribute.UserPrincipalNotFoundException
import java.nio.file.{ FileAlreadyExistsException, Path }
import java.util.UUID

import better.files.File.temporaryDirectory
import better.files.{ Dispose, File }
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.authentication.LdapAuthentication
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, StateInfo }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

class EasyDepositApiApp(configuration: Configuration) extends DebugEnhancedLogging
  with LdapAuthentication
  with PidRequesterComponent {

  val properties: PropertiesConfiguration = configuration.properties

  override val pidRequester: PidRequester = new PidRequester with HttpContext {
    override val pidGeneratorService: URI = new URI(properties.getString("pids.generator-service"))
    override val applicationVersion: String = configuration.version
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
  }

  def getVersion: String = {
    configuration.version
  }

  private val uploadStagingDir = {
    val dir = getConfiguredDirectory("deposits.stage.upload")
    logger.info(s"Uploads are staged in $dir")
    if (dir.nonEmpty) {
      val msg = s"Pending uploads were probably interrupted. See logs lines with 'POST /deposit/{id}/file/{dir_path}'. Please remove their directories from the staging area: $dir"
      logger.error(msg)
      throw new FileAlreadyExistsException(msg)
    }
    dir
  }
  private val draftsDir = getConfiguredDirectory("deposits.drafts")
  private val stagingDir = getConfiguredDirectory("deposits.stage")
  private val submitToDir = getConfiguredDirectory("deposits.submit-to")

  private val group = configuration.properties.getString("deposit.permissions.group")
  private val principal = Try {
    stagingDir.path.getFileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(group)
  }.getOrRecover {
    case e: UserPrincipalNotFoundException => throw new IOException(s"Group $group could not be found", e)
    case e: UnsupportedOperationException => throw new IOException("Not on a POSIX supported file system", e)
    case NonFatal(e) => throw new IOException(s"unexpected error occured on $stagingDir", e)
  }
  private lazy val submitter = new Submitter(stagingDir, submitToDir, principal)

  private def getConfiguredDirectory(key: String): File = {
    val dir = File(configuration.properties.getString(key))

    if (!dir.exists) throw ConfigurationException(s"Configured directory '$key' does not exist: $dir")
    if (!dir.isDirectory) throw ConfigurationException(s"Configured directory '$key' is a regular file: $dir")
    if (!dir.isReadable) throw ConfigurationException(s"Configured directory '$key' is not readable: $dir")

    dir
  }

  private def getDeposit(user: String, id: UUID): Try[DepositDir] = {
    DepositDir.get(draftsDir, user, id)
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
      deposit <- getDeposit(user, id)
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
    deposit <- getDeposit(user, id)
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
    deposit <- getDeposit(user, id)
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
    deposit <- getDeposit(user, id)
    doi <- deposit.getDOI(pidRequester)
  } yield doi

  /**
   * @param user the user ID
   * @param id   the deposit ID
   * @return Failure(BadDoiException) if the DOI in the DatasetMetadata does not equal the DOI in the dataset properties (both may be absent)
   */
  def checkDoi(user: String, id: UUID, dm: DatasetMetadata): Try[Unit] = for {
    deposit <- getDeposit(user, id)
    _ <- deposit.sameDOIs(dm)
  } yield ()

  /**
   * Returns the dataset metadata from `dataset.xml`.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @return
   */
  def getDatasetMetadataForDeposit(user: String, id: UUID): Try[DatasetMetadata] = for {
    deposit <- getDeposit(user, id)
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
    deposit <- getDeposit(user, id)
    _ <- deposit.writeDatasetMetadataJson(dm)
  } yield ()

  /**
   * Returns a list of [[nl.knaw.dans.easy.deposit.docs.FileInfo]] objects if the path points to a directory,
   * else a single FileInfo object
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @param path path to a directory or a file
   * @return
   */
  def getFileInfo(user: String, id: UUID, path: Path): Try[Object] = for {
    dataFiles <- getDataFiles(user, id)
    contents <- if (dataFiles.isDirectory(path)) dataFiles.list(path)
                else dataFiles.get(path)
  } yield contents

  /**
   * Returns dataset files of the deposit
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @return
   */
  private def getDataFiles(user: String, id: UUID): Try[DataFiles] = for {
    deposit <- getDeposit(user, id)
    dataFiles <- deposit.getDataFiles
  } yield dataFiles

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
  def writeDepositFile(is: => InputStream, user: String, id: UUID, path: Path): Try[Boolean] = {
    for {
      dataFiles <- getDataFiles(user, id)
      _ = logger.info(s"uploading to [${ dataFiles.bag.baseDir }] of [$path]")
      created <- dataFiles.write(is, path)
      _ = logger.info(s"created=$created $user/$id/$path")
    } yield created
  }

  def stageFiles(userId: String, id: UUID, destination: Path): Try[(Dispose[File], StagedFilesTarget)] = {
    val prefix = s"$userId-$id-"
    for {
      deposit <- DepositDir.get(draftsDir, userId, id)
      dataFiles <- deposit.getDataFiles
      stagingDir <- createManagedTempDir(prefix)
      _ <- atMostOneTempDir(prefix).doIfFailure { case _ => stagingDir.get() }
    } yield (stagingDir, StagedFilesTarget(dataFiles.bag, destination))
  }

  // the directory is dropped when the resource is released
  private def createManagedTempDir(prefix: String): Try[Dispose[File]] = Try {
    temporaryDirectory(prefix, Some(uploadStagingDir.createDirectories()))
  }

  // prevents concurrent uploads, requires explicit cleanup of interrupted uploads
  private def atMostOneTempDir(prefix: String): Try[Unit] = {
    if (uploadStagingDir.list.count(_.name.startsWith(prefix)) > 1)
      Failure(ConcurrentUploadException(prefix))
    else Success(())
  }

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
    dataFiles <- getDataFiles(user, id)
    _ <- dataFiles.delete(path)
  } yield ()
}
