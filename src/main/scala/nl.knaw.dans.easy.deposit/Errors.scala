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

import java.nio.file.{ FileAlreadyExistsException, Path }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import nl.knaw.dans.easy.deposit.servlets.contentTypePlainText
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.servlet.FileItem
import org.scalatra.{ ActionResult, InternalServerError }

import scala.util.Try

object Errors extends DebugEnhancedLogging {

  case class ConfigurationException(msg: String) extends IllegalArgumentException(s"Configuration error: $msg")

  case class LeftoversOfForcedShutdownException(dir: File)
    extends FileAlreadyExistsException(
      s"Staging area [$dir] should be empty unless a forced shutdown occurred during an upload/submit request." +
        " Directories containing bags are aborted submits, somehow (allow) resubmit if indeed not in fedora." +
        " Other directories contain files of uploads that are not yet moved into the bag."
    )

  abstract sealed class ServletResponseException(status: Int, httpResponseBody: String)
    extends Exception(httpResponseBody) {
    val internalServerError: Boolean = status == INTERNAL_SERVER_ERROR_500

    def getActionResult: ActionResult = ActionResult(status, httpResponseBody, Map(contentTypePlainText))
  }

  implicit class TriedActionResult(val t: Try[ActionResult]) extends AnyVal {
    def getOrRecoverWithActionResult: ActionResult = t.getOrRecover {
      case e: ServletResponseException if !e.internalServerError => e.getActionResult
      case e => // this also handles implicitly not expected exceptions
        logger.error(s"Not expected exception: ${ e.getMessage }", e)
        InternalServerError("Internal Server Error", Map(contentTypePlainText))
    }
  }

  case class OverwriteException(httpResponseBody: String)
    extends ServletResponseException(CONFLICT_409, httpResponseBody)

  case class InvalidResourceException(httpResponseBody: String)
    extends ServletResponseException(NOT_FOUND_404, httpResponseBody)

  case class CorruptDepositException(user: String, id: String, cause: Throwable)
    extends ServletResponseException(INTERNAL_SERVER_ERROR_500, s"Invalid deposit uuid $id for user $user: ${ cause.getMessage }") {
    logger.error(cause.getMessage)
  }

  class PropertyException(msg: String)
    extends ServletResponseException(INTERNAL_SERVER_ERROR_500, msg)

  case class PropertyNotFoundException(key: String, props: PropertiesConfiguration)
    extends PropertyException(s"no value for '$key' in ${ Option(props.getFile).getOrElse("not found properties file") }")

  case class InvalidPropertyException(key: String, value: String, props: PropertiesConfiguration)
    extends PropertyException(s"Not expected value '$value' for '$key' in ${ props.getFile }")

  case class IllegalStateTransitionException(oldState: State, newState: State)
    extends ServletResponseException(FORBIDDEN_403, s"Cannot transition from $oldState to $newState")

  case class IllegalDepositStateException(action: String, actual: State, allowed: Seq[State])
    extends ServletResponseException(
      FORBIDDEN_403,
      s"Deposit has state $actual, can only $action deposits with one of the states: ${ allowed.mkString(", ") }"
    )

  case class ZipMustBeOnlyFileException(item: FileItem)
    extends ServletResponseException(
      BAD_REQUEST_400,
      s"A multipart/form-data message contained a ZIP part [${ item.name }] but also other parts."
    )

  case class InvalidDocumentException(document: String, t: Throwable = null)
    extends ServletResponseException(
      BAD_REQUEST_400,
      if (t == null) s"invalid $document"
      else s"invalid $document: ${ t.getMessage }"
    )

  /** Note: submit area == easy-ingest-flow-inbox */
  case class AlreadySubmittedException(uuid: UUID)
    extends ServletResponseException(
      INTERNAL_SERVER_ERROR_500,
      s"The submit-id (UUID $uuid) already exists in the submit area."
    )

  case class InvalidDoiException(uuid: UUID)
    extends ServletResponseException(BAD_REQUEST_400, s"InvalidDoi: DOI must be obtained by calling GET /deposit/$uuid")

  case class MalformedZipException(msgAboutEntry: String)
    extends ServletResponseException(BAD_REQUEST_400, s"ZIP file is malformed. $msgAboutEntry")

  case class PendingUploadException()
    extends ServletResponseException(CONFLICT_409, "Another upload or submit is pending.")

  case class NoStagingDirException(file: File)
    extends ServletResponseException(INTERNAL_SERVER_ERROR_500, s"Staging directory was not created: $file")

  case class ClientAbortedUploadException(path: String)
    extends ServletResponseException(OK_200, s"Client aborted upload of path $path") {
    logger.info(getMessage) // logging the body explains why the request did not log new payloads
  }

  case class InvalidContentTypeException(contentType: Option[String], requirement: String)
    extends ServletResponseException(BAD_REQUEST_400, contentType
      .map(s => s"Content-Type $requirement Got: $s")
      .getOrElse(s"Content-Type is a mandatory request header and $requirement")
    )

  case class NoSuchDepositException(user: String, id: UUID, cause: Throwable)
    extends ServletResponseException(NOT_FOUND_404, s"Deposit $id not found") {
    logger.info(s"Deposit [$user/$id] not found: ${ cause.getMessage }")
  }

  case class NoSuchFileInDepositException(absPath: File, relPath: Path)
    extends ServletResponseException(NOT_FOUND_404, s"$relPath not found in deposit") {
    logger.info(s"$relPath not found")
  }
}
