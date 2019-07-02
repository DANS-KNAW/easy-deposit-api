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
import java.net.{ URI, URL }
import java.nio.file.Path
import java.util.UUID

import better.files.File.temporaryDirectory
import better.files.{ Dispose, File }
import nl.knaw.dans.easy.deposit.Errors.{ ClientAbortedUploadException, ConfigurationException, CorruptUserException, InvalidContentTypeException, LeftoversOfForcedShutdownException, NoStagingDirException, OverwriteException, PendingUploadException }
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.authentication.{ AuthenticationProvider, LdapAuthentication }
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, StateInfo }
import nl.knaw.dans.easy.deposit.servlets.contentTypeZipPattern
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.io.EofException
import org.joda.time.DateTime
import org.scalatra.servlet.MultipartConfig

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
  override val authentication: AuthenticationProvider = new Authentication {
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

  private val stagedBaseDir = {
    val dir = getConfiguredDirectory("deposits.staged")
    if (dir.nonEmpty) throw LeftoversOfForcedShutdownException(dir)
    logger.info(s"Uploads/submits will be staged in $dir")
    dir
  }
  private val draftBase: File = getConfiguredDirectory("deposits.drafts")
  private val submitBase: File = getConfiguredDirectory("deposits.submit-to")
  StartupValidation.allowsAtomicMove(srcDir = stagedBaseDir, targetDir = draftBase)

  val multipartConfig: MultipartConfig = {
    val multipartLocation = getConfiguredDirectory("multipart.location")
    logger.info(s"Uploads are received at multipart.location: $multipartLocation")
    StartupValidation.allowsAtomicMove(srcDir = multipartLocation, targetDir = stagedBaseDir)
    MultipartConfig(
      location = Some(multipartLocation.toString()),
      maxFileSize = Option(properties.getLong("multipart.max-file-size", null)),
      maxRequestSize = Option(properties.getLong("multipart.max-request-size", null)),
      fileSizeThreshold = Some(properties.getInt("multipart.file-size-threshold")), //throws if not provided
    )
  }

  private val submitter = {
    val groupName = properties.getString("deposit.permissions.group")
    new Submitter(stagedBaseDir, submitBase, groupName)
  }

  // possible trailing slash is dropped
  private val easyHome: URL = new URL(properties.getString("easy.home").replaceAll("/?$", ""))

  @throws[ConfigurationException]("when no existing readable directory is configured")
  private def getConfiguredDirectory(key: String): File = {
    val str = Option(properties.getString(key))
      .getOrElse(throw ConfigurationException(s"No configuration value for $key"))
    val dir = File(str)

    if (!dir.exists) throw ConfigurationException(s"Configured directory '$key' does not exist: $dir")
    if (!dir.isDirectory) throw ConfigurationException(s"Configured directory '$key' is a regular file: $dir")
    if (!dir.isReadable) throw ConfigurationException(s"Configured directory '$key' is not readable: $dir")

    dir
  }

  private def getDeposit(user: String, id: UUID): Try[DepositDir] = {
    DepositDir.get(draftBase, user, id)
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
    DepositDir.create(draftBase, user).flatMap(_.getDepositInfo(submitBase, easyHome))
  }

  /**
   * Returns the list of `DepositInfo` objects for the deposits in the user's draft area.
   *
   * @param user the user ID
   * @return a list of [[docs.DepositInfo]] objects
   */
  def getDeposits(user: String): Try[Seq[DepositInfo]] = {
    implicit val timestampOrdering: Ordering[DateTime] = Ordering.fromLessThan[DateTime](_ isBefore _)
    val deposits = DepositDir.list(draftBase, user)
    for {
      infos <- deposits.map(_.getDepositInfo(submitBase, easyHome)).collectResults
      sortedInfos = infos.sortBy(deposit => (deposit.state, deposit.date))
    } yield sortedInfos
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
      state <- deposit.getStateManager(submitBase, easyHome).getStateInfo
    } yield state
  }

  /**
   * Sets the deposit state. The only legal transitions are:
   *
   * - from [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.draft]] to [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.submitted]]
   * - from [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.rejected]] to [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.draft]]
   *
   * Any attempt at another transition will result in an [[nl.knaw.dans.easy.deposit.Errors.IllegalStateTransitionException]].
   *
   * When transitioning to [[nl.knaw.dans.easy.deposit.docs.StateInfo.State.submitted]] the following steps will be executed:
   *
   * 1. The `files.xml` of the bag will be written.
   * 2. The SHA-1 payload manifest will be calculated and written.
   * 3. The deposit directory will be copied to the staging area.
   * 4. The copy will be moved to the deposit area.
   *
   * @param user         the user ID
   * @param id           the deposit ID
   * @param newStateInfo the state to transition to
   * @return
   */
  def setDepositState(newStateInfo: StateInfo, user: String, id: UUID): Try[Unit] = {
    def submit(deposit: DepositDir, stateManager: StateManager) = {
      for {
        fullName <- getFullName(user)
        dispoableStagedDir <- getStagedDir(user, id)
        _ <- dispoableStagedDir.apply(submitter.submit(deposit, stateManager, fullName, _))
      } yield ()
    }

    for {
      deposit <- getDeposit(user, id)
      stateManager = deposit.getStateManager(submitBase, easyHome)
      _ <- stateManager.canChangeState(newStateInfo)
      _ <- if (newStateInfo.state == State.submitted)
             submit(deposit, stateManager) // also changes the state
           else stateManager.changeState(newStateInfo)
    } yield ()
  }

  private def getFullName(user: String): Try[String] = {
    getUser(user)
      .map(_ ("displayName").headOption.getOrElse(throw CorruptUserException(s"$user has no displayName")))
  }


  /**
   * Deletes the deposit from the user's draft area.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @return
   */
  def deleteDeposit(user: String, id: UUID): Try[Unit] = for {
    deposit <- getDeposit(user, id)
    state <- deposit.getStateManager(submitBase, easyHome).getStateInfo
    _ <- state.canDelete
    _ = deposit.bagDir.parent.delete()
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
    _ <- canUpdate(user, id)
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
   * @param user        the user ID
   * @param id          the deposit ID
   * @param path        the path of the file to (over)write, relative to the content base directory
   * @param is          the input stream to write from
   * @param contentType the contentType of the stream, mandatory in this context
   *                    but optional by the nature of an HTTP request header
   * @return `true` if a new file was created, `false` otherwise
   */
  def writeDepositFile(is: => InputStream, user: String, id: UUID, path: Path, contentType: Option[String]): Try[Boolean] = {
    def writeDataFile(dataFiles: DataFiles, lockDir: Dispose[File]): Try[Boolean] = {
      dataFiles
        .write(is, path)
        .doIfFailure { case _ => releaseLock(lockDir) }
        .recoverWith { case _: EofException => Failure(ClientAbortedUploadException(s"$user/$id/$path")) }
    }

    for {
      _ <- contentTypeAnythingBut(contentType)
      dataFiles <- getDataFiles(user, id)
      _ <- canUpdate(user, id)
      _ <- pathNotADirectory(path, dataFiles)
      disposableLockDir <- getStagedDir(user, id)
      _ = logger.info(s"uploading to [${ dataFiles.bag.baseDir }] of [$path]")
      created <- writeDataFile(dataFiles, disposableLockDir)
      _ = logger.info(s"created=$created $user/$id/$path")
      _ = releaseLock(disposableLockDir)
    } yield created
  }

  private def releaseLock(lockDir: Dispose[File]) = {
    lockDir.get().delete(swallowIOExceptions = true)
  }

  private def pathNotADirectory(path: Path, dataFiles: DataFiles): Try[Unit] = {
    if ((dataFiles.bag / "data" / path.toString).isDirectory)
      Failure(OverwriteException("Attempt to overwrite a directory with a file."))
    else Success(())
  }

  private def contentTypeAnythingBut(contentType: Option[String]): Try[Unit] = {
    contentType.map(_.trim.toLowerCase) match {
      case Some(str) if str.nonEmpty
        && !str.startsWith("multipart")
        && !str.matches(contentTypeZipPattern) => Success(())
      case _ => Failure(InvalidContentTypeException(contentType, "must not be application/zip nor start with multipart."))
    }
  }

  def stageFiles(userId: String, id: UUID, destination: Path): Try[(Dispose[File], StagedFilesTarget)] = {
    for {
      _ <- canUpdate(userId, id)
      deposit <- DepositDir.get(draftBase, userId, id)
      dataFiles <- deposit.getDataFiles
      disposableStagingDir <- getStagedDir(userId, id)
    } yield (disposableStagingDir, StagedFilesTarget(dataFiles.bag, destination))
  }

  private def getStagedDir(userId: String, id: UUID): Try[Dispose[File]] = {
    // side effect: optimistic lock for a deposit
    val prefix = s"$userId-$id-"
    for {
      disposableStagingDir <- createManagedTempDir(prefix)
      _ <- atMostOneTempDir(prefix).doIfFailure { case _ => disposableStagingDir.get() }
    } yield disposableStagingDir

  }

  // the temporary directory is dropped when the disposable resource is released on completion of the request,
  // unless the directory was moved away before to ingest-flow-inbox
  private def createManagedTempDir(prefix: String): Try[Dispose[File]] = Try {
    temporaryDirectory(prefix, Some(stagedBaseDir.createDirectories()))
  }

  // prevents concurrent uploads to a single draft deposit, requires explicit cleanup of interrupted uploads
  private def atMostOneTempDir(prefix: String): Try[Unit] = {
    stagedBaseDir.list.count(_.name.startsWith(prefix)) match {
      case 0 => Failure(NoStagingDirException(stagedBaseDir / prefix))
      case 1 => Success(())
      case _ => Failure(PendingUploadException())
    }
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
    _ <- canUpdate(user, id) //  deleting a file, is updating the deposit
    _ <- dataFiles.delete(path)
  } yield ()

  /**
   * Determines if the deposit can be updated, based on its current state.
   *
   * @param user ID
   * @param id   the deposit ID
   * @return
   */
  private def canUpdate(user: String, id: UUID): Try[Unit] = {
    getDepositState(user, id).flatMap(_.canUpdate)
  }
}
