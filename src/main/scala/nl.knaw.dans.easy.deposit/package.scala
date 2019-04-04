/**
 * requirement(C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
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
package nl.knaw.dans.easy

import java.nio.file.{ Path, Paths }
import java.util.UUID

import better.files.{ File, StringOps }
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.servlet.FileItem

import scala.util.{ Failure, Try }
import scala.xml._

package object deposit extends DebugEnhancedLogging {

  class ForbiddenException(httpResponseBody: String) extends Exception(httpResponseBody)
  class BadRequestException(httpResponseBody: String) extends Exception(httpResponseBody)
  class ConflictException(httpResponseBody: String) extends Exception(httpResponseBody)
  class NotFoundException(httpResponseBody: String) extends Exception(httpResponseBody)
  case class ExistsException(httpResponseBody: String) extends ConflictException(httpResponseBody)

  case class CorruptDepositException(user: String, id: String, cause: Throwable)
    extends Exception(s"Invalid deposit uuid $id for user $user: ${ cause.getMessage }", cause)

  case class IllegalStateTransitionException(oldState: State, newState: State)
    extends ForbiddenException(s"Cannot transition from $oldState to $newState")

  case class IllegalDepositStateException(action: String, actual: State, allowed: Seq[State])
    extends ForbiddenException(s"Deposit has state $actual, can only $action deposits with one of the states: ${ allowed.mkString(", ") }")

  case class ZipMustBeOnlyFileException(item: FileItem)
    extends BadRequestException(s"A multipart/form-data message contained a ZIP part [${ item.name }] but also other parts.")

  /**
   * Note 1: submit area == ingest-flow-inbox
   * Note 2: Resubmit may follow a reject, be a concurrent submit request or ...
   * The end user can compare the UUID with the URL of a deposit.
   * The UUID can help communication with trouble shooters.
   */
  case class AlreadySubmittedException(uuid: UUID) extends ConflictException(s"The deposit (UUID $uuid) already exists in the submit area. Possibly due to a resubmit.")
  case class InvalidDoiException(uuid: UUID) extends BadRequestException(s"InvalidDoi: DOI must be obtained by calling GET /deposit/$uuid")
  case class MalformedZipException(msgAboutEntry: String) extends BadRequestException(s"ZIP file is malformed. $msgAboutEntry")
  case class PendingUploadException() extends ConflictException("Another upload is pending. Please try again later.")
  case class InvalidContentTypeException(contentType: Option[String], requirement: String) extends BadRequestException(
    contentType match {
      case None => s"Content-Type is a mandatory request header and $requirement"
      case Some(s) => s"Content-Type $requirement Got: $s"
    }
  )
  case class NoSuchDepositException(user: String, id: UUID, cause: Throwable) extends NotFoundException(s"Deposit $id not found") {
    logger.info(s"Deposit [$user/$id] not found: ${ cause.getMessage }")
  }
  case class NoSuchFileInDepositException(absPath: File, relPath: Path) extends NotFoundException(s"$relPath not found in deposit") {
    logger.info(s"$relPath not found")
  }

  case class ConfigurationException(msg: String) extends IllegalArgumentException(s"Configuration error: $msg")

  val prologue = """<?xml version='1.0' encoding='UTF-8'?>"""

  implicit class XmlExtensions(val elem: Elem) extends AnyVal {

    def serialize: String = {
      val printer = new PrettyPrinter(160, 2)
      val trimmed = Utility.trim(elem)
      prologue + "\n" + printer.format(trimmed)
    }
  }
  implicit class BagExtensions(val bag: DansBag) extends AnyVal {
    def addMetadataFile(content: Elem, target: String): Try[Any] = {
      bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/$target"))
    }

    def addMetadataFile(content: String, target: String): Try[Any] = {
      bag.addTagFile(content.inputStream, Paths.get(s"metadata/$target"))
    }
  }
  implicit class FailFastStream[T](val stream: Stream[Try[T]]) extends AnyVal {
    // TODO candidate for nl.knaw.dans.lib.error ?
    def failFastOr[R](onSuccess: => Try[R]): Try[R] = {
      stream
        .collectFirst { case Failure(e) => Failure(e) }
        .getOrElse(onSuccess)
    }
  }
}
