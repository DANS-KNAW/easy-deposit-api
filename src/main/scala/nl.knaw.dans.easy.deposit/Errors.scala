package nl.knaw.dans.easy.deposit

import java.nio.file.Path
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.servlet.FileItem

object Errors {

  class ForbiddenException(httpResponseBody: String) extends Exception(httpResponseBody)
  class BadRequestException(httpResponseBody: String) extends Exception(httpResponseBody)
  class ConflictException(httpResponseBody: String) extends Exception(httpResponseBody)
  class NotFoundException(httpResponseBody: String) extends Exception(httpResponseBody)

  case class OverwriteException(httpResponseBody: String) extends ConflictException(httpResponseBody)
  case class InvalidResourceException(httpResponseBody: String) extends NotFoundException(httpResponseBody)

  case class CorruptDepositException(user: String, id: String, cause: Throwable)
    extends Exception(s"Invalid deposit uuid $id for user $user: ${ cause.getMessage }", cause)

  case class IllegalStateTransitionException(oldState: State, newState: State)
    extends ForbiddenException(s"Cannot transition from $oldState to $newState")

  case class IllegalDepositStateException(action: String, actual: State, allowed: Seq[State])
    extends ForbiddenException(s"Deposit has state $actual, can only $action deposits with one of the states: ${ allowed.mkString(", ") }")

  case class ZipMustBeOnlyFileException(item: FileItem)
    extends BadRequestException(s"A multipart/form-data message contained a ZIP part [${ item.name }] but also other parts.")

  case class InvalidDocumentException(document: String, t: Throwable = null)
    extends BadRequestException(if (t == null) s"invalid $document"
                                else s"invalid $document: ${ t.getMessage }")

  /**
   * Note 1: submit area == easy-ingest-flow-inbox
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
  case class NoSuchDepositException(user: String, id: UUID, cause: Throwable) extends NotFoundException(s"Deposit $id not found") with DebugEnhancedLogging {
    logger.info(s"Deposit [$user/$id] not found: ${ cause.getMessage }")
  }
  case class NoSuchFileInDepositException(absPath: File, relPath: Path) extends NotFoundException(s"$relPath not found in deposit") with DebugEnhancedLogging {
    logger.info(s"$relPath not found")
  }

  case class ConfigurationException(msg: String) extends IllegalArgumentException(s"Configuration error: $msg")
}
