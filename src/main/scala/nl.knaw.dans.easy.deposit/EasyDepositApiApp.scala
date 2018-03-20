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
import java.nio.file.{ Path, Paths }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.authentication.LdapAuthentication
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatra.CookieOptions
import pdi.jwt.{ JwtAlgorithm, JwtOptions }
import pdi.jwt.algorithms.JwtHmacAlgorithm

import scala.util.Try
class EasyDepositApiApp(configuration: Configuration) extends DebugEnhancedLogging
  with LdapAuthentication {
  private val properties: PropertiesConfiguration = configuration.properties

  private def toHmacAlgorithm(value: String): JwtHmacAlgorithm = {
    // TODO confine use of the library to TokenSupport for easy replacement in case of trouble with the library
    Try {
      JwtAlgorithm.fromString(value).asInstanceOf[JwtHmacAlgorithm]
    }.getOrRecover { t => throw new Exception(s"asymmetrical or unknown JwtHmacAlgorithm configured [$value]: $t") }
  }
  private val expiresIn: Int = properties.getInt("auth.cookie.expiresIn", 10)

class EasyDepositApiApp(configuration: Configuration) {
  val tokenConfig = TokenConfig(
    secretKey = properties.getString("auth.jwt.secret.key", "test"), // TODO Change type to SecretKey? Really in application.properties?
    expiresIn = expiresIn, // seconds
    algorithm = toHmacAlgorithm(properties.getString("auth.jwt.hmac.algorithm", "HS256")),
    options = JwtOptions(leeway = 10) // JWT lives 10 seconds longer than cookie
  )
  logger.info(s"tokenConfig: $tokenConfig")

  val authCookieOptions: CookieOptions = CookieOptions(
    domain = "", // limits which server get the cookie // TODO by default the host who sent it?
    path = "/", // limits which route gets the cookie, TODO configure and/or from mounts in Service class
    maxAge = expiresIn, // seconds
    secure = false, // TODO true when service supports HTTPS to prevent browsers to send it over http
    httpOnly = true, // JavaScript can't get the cookie
    // version = 0 // obsolete? https://stackoverflow.com/questions/29124177/recommended-set-cookie-version-used-by-web-servers-0-1-or-2#29143128
  )
  logger.info(s"authCookieOptions: $authCookieOptions")

  override val authentication: Authentication = new Authentication {
    override val ldapUserIdAttrName: String = properties.getString("users.ldap-user-id-attr-name")
    override val ldapParentEntry: String = properties.getString("users.ldap-parent-entry")
    override val ldapProviderUrl: String = properties.getString("users.ldap-url")
    logger.info(s"Authentication: ldapProviderUrl = $ldapProviderUrl")
    logger.info(s"Authentication: ldapParentEntry = $ldapParentEntry")
    logger.info(s"Authentication: ldapUserIdAttrName = $ldapUserIdAttrName")
  }

  def getVersion: String = {
    configuration.version
  }

  private val draftsDir = getConfiguredDirectory("deposits.drafts")
  private val submitter = new Submitter(
    stagingBaseDir = getConfiguredDirectory("deposits.stage"),
    submitToBaseDir = getConfiguredDirectory("deposits.submit-to"))

  private def getConfiguredDirectory(key: String): File = {
    val dir = File(configuration.properties.getString("deposits.drafts"))

    if (!dir.exists) throw ConfigurationException(s"Configured directory '$key' does not exist: $dir")
    if (!dir.isDirectory) throw ConfigurationException(s"Configured directory '$key' is a regular file: $dir")
    if (!dir.isReadable) throw ConfigurationException(s"Configured directory '$key' is not readable: $dir")

    dir
  }

  /**
   * Creates a new, empty deposit, containing an empty bag in the user's draft area. If the user
   * has no draft area yet, it is first created.
   *
   * @param user the user ID
   * @return the new deposit's ID
   */
  def createDeposit(user: String): Try[UUID] = {
    DepositDir.create(draftsDir, user).map(_.id)
  }

  /**
   * Returns the list of `DepositInfo` objects for the deposits in the user's draft area.
   *
   * @param user the user ID
   * @return a list of [[DepositInfo]] objects
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
   * - from [[State.DRAFT]] to [[State.SUBMITTED]]
   * - from [[State.REJECTED]] to [[State.DRAFT]]
   *
   * Any attempt at another transition will result in an [[nl.knaw.dans.easy.deposit.IllegalStateTransitionException]].
   *
   * When transitioning to [[State.SUBMITTED]] the following steps will be executed:
   *
   * 1. The `files.xml` of the bag will be written.
   * 2. The SHA-1 payload manifest will be calculated and written.
   * 3. The deposit directory will be copied to the staging area.
   * 4. The copy will be moved to the deposit area.
   *
   * @param user  the user ID
   * @param id    the deposit ID
   * @param state the state to transition to
   * @return
   */
  // TODO: do post processing described above in a worker thread so that submit can return fast for large deposits.
  def setDepositState(state: StateInfo)(user: String, id: UUID): Try[Unit] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    _ <- if (state.state == State.SUBMITTED) submitter.submit(deposit)
         else deposit.setStateInfo(state)
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
   * Writes the provided [[DatasetMetadata]] object as `dataset.xml` to the deposit directory. Any
   * existing `dataset.xml` is overwritten, so it is important that the object contains the complete
   * current metadata.
   *
   * @param user the user ID
   * @param id   the deposit ID
   * @param dm   the dataset metadata
   * @return
   */
  def writeDataMetadataToDeposit(dm: DatasetMetadata)(user: String, id: UUID): Try[Unit] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    _ <- deposit.setDatasetMetadata(dm)
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
  def writeDepositFile(is: => InputStream)(user: String, id: UUID, path: Path): Try[Boolean] = for {
    deposit <- DepositDir.get(draftsDir, user, id)
    dataFiles <- deposit.getDataFiles
    created <- dataFiles.write(is, path)
  } yield created

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
