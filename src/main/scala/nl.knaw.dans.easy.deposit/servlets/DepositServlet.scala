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
package nl.knaw.dans.easy.deposit.servlets

import java.io.InputStream
import java.nio.charset.Charset
import java.nio.file.{ NoSuchFileException, Path, Paths }
import java.util.UUID
import java.util.zip.{ ZipEntry, ZipException, ZipInputStream }

import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, toJson }
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, StateInfo }
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.{ BadRequestException, InvalidResourceException }
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import org.apache.commons.lang.NotImplementedException
import org.scalatra._
import org.scalatra.servlet.{ FileItem, FileUploadSupport, MultipartConfig, SizeConstraintExceededException }
import org.scalatra.util.RicherString._
import resource.managed

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp)
  extends ProtectedServlet(app)
    with FileUploadSupport {
  configureMultipartHandling(MultipartConfig())
  error {
    case e: SizeConstraintExceededException => RequestEntityTooLarge(s"too much! ${ e.getMessage }")
  }
  /*
   * Defensive programming convention at this top level, everything in a for-comprehension:
   *
   *    (for {...} yield ...).getOrRecoverResponse(respond)
   *
   * Thus programming errors of the type that throws an exception despite returning a try
   * won't cause a stack trace.
   *
   * A unit test emulating a programming error in a called method, something like
   *
   *    (mockedApp.Xxx) expects ... throwing new Exception("someone made a programming error")
   *
   * showed this doesn't work with just one assignment within the for-comprehension.
   * Anyhow, the implicit `user.id` is not safe outside a for-comprehension as (in theory) it can be null.
   */

  get("/") {
    (for {
      userId <- getUserId
      deposits <- app.getDeposits(userId)
    } yield Ok(body = toJson(deposits)))
      .getOrRecoverResponse(respond)
  }
  post("/") {
    (for {
      userId <- getUserId
      depositInfo <- app.createDeposit(userId)
    } yield depositCreatedResponse(depositInfo))
      .getOrRecoverResponse(respond)
  }
  get("/:uuid/metadata") {
    (for {
      uuid <- getUUID
      dmd <- app.getDatasetMetadataForDeposit(user.id, uuid)
    } yield Ok(body = toJson(dmd))
      ).getOrRecoverResponse(respond)
  }
  get("/:uuid/doi") {
    (for {
      uuid <- getUUID
      doi <- app.getDoi(user.id, uuid)
    } yield Ok(body = s"""{"doi":"$doi"}""")
      ).getOrRecoverResponse(respond)
  }
  put("/:uuid/metadata") {
    (for {
      uuid <- getUUID
      managedIS <- getRequestBodyAsManagedInputStream
      datasetMetadata <- managedIS.apply(is => DatasetMetadata(is))
      _ <- app.writeDataMetadataToDeposit(datasetMetadata, user.id, uuid)
    } yield NoContent()
      ).getOrRecoverResponse(respond)
  }
  get("/:uuid/state") {
    (for {
      uuid <- getUUID
      depositState <- app.getDepositState(user.id, uuid)
    } yield Ok(body = toJson(depositState))
      ).getOrRecoverResponse(respond)
  }
  put("/:uuid/state") {
    (for {
      uuid <- getUUID
      managedIS <- getRequestBodyAsManagedInputStream
      stateInfo <- managedIS.apply(is => StateInfo(is))
      _ <- app.setDepositState(stateInfo, user.id, uuid)
    } yield NoContent()
      ).getOrRecoverResponse(respond)
  }
  delete("/:uuid") {
    (for {
      uuid <- getUUID
      _ <- app.deleteDeposit(user.id, uuid)
    } yield NoContent()
      ).getOrRecoverResponse(respond)
  }
  get("/:uuid/file/*") { //dir and file
    (for {
      uuid <- getUUID
      path <- getPath
      depositFiles <- app.getDepositFiles(user.id, uuid, path)
    } yield Ok(body = toJson(depositFiles))
      ).getOrRecoverResponse(respond)
  }
  post("/:uuid/file/*") { //file(s)
    (for {
      uuid <- getUUID
      path <- getPath
      _ <- isMultipart
      newFileItems <- getFileItems
        .toStream
        .withFilter(_.name.blankOption.isDefined)
        .map(uploadFileItem(uuid, path, _))
        .failFast
        .map(_.withFilter(_._1).map(_._2))
    } yield Ok() // TODO create multiple location headers from newFileItems?
      ).getOrRecoverResponse(respond)
  }
  put("/:uuid/file/*") { //file
    (for {
      uuid <- getUUID
      path <- getPath
      managedIS <- getRequestBodyAsManagedInputStream
      newFileWasCreated <- managedIS.apply(app.writeDepositFile(_, user.id, uuid, path))
    } yield if (newFileWasCreated)
              Created(headers = Map("Location" -> request.uri.toASCIIString))
            else Ok()
      ).getOrRecoverResponse(respond)
  }
  delete("/:uuid/file/*") { //dir and file
    (for {
      uuid <- getUUID
      path <- getPath
      _ <- app.deleteDepositFile(user.id, uuid, path)
    } yield NoContent()
      ).getOrRecoverResponse(respond)
  }

  private def uploadFileItem(uuid: UUID, path: Path, uploadItem: FileItem): Try[(Boolean, FileItem)] = {
    managed(uploadItem.getInputStream).apply[Try[_]] {
      case  is if isZip(uploadItem) => app.unzipDepositFile(is, uploadItem.charset, user.id, uuid, path)
      case  is => app.writeDepositFile(is, user.id, uuid, path.resolve(uploadItem.name))
    }.map(_ => (false, uploadItem))
  }

  private def isZip(uploadItem: FileItem) = {
    val extensionIsZip = uploadItem.name.matches(".*.g?z(ip)?")
    lazy val contentTypeIsZip = uploadItem.contentType.exists(_.matches(
      "(application|multipart)/(x-)?g?zip(-compress(ed)?)?( .*)?"
    ))
    logger.debug(s"ZIP check: ${ uploadItem.name } : $extensionIsZip; ${ uploadItem.contentType } : $contentTypeIsZip ")
    extensionIsZip || contentTypeIsZip
  }

  private def respond(t: Throwable): ActionResult = t match {
    case e: IllegalStateTransitionException => Forbidden(e.getMessage)
    case e: NoSuchDepositException => noSuchDepositResponse(e)
    case e: NoSuchFileException => NotFound(body = s"${ e.getMessage } not found")
    case e: InvalidResourceException => invalidResourceResponse(e)
    case e: InvalidDocumentException => badDocResponse(e)
    case e: BadRequestException => BadRequest(e.getMessage)
    case e: NotImplementedException => NotImplemented(e.getMessage)
    case _ => internalErrorResponse(t)
  }

  private def noSuchDepositResponse(e: NoSuchDepositException): ActionResult = {
    // returning the message to the client might reveal absolute paths on the server
    logWhatIsHiddenForGetOrRecoverResponse(e.getMessage)
    NotFound(body = s"Deposit ${ e.id } not found")
  }

  private def invalidResourceResponse(t: InvalidResourceException): ActionResult = {
    // returning the message to the client might reveal absolute paths on the server
    logWhatIsHiddenForGetOrRecoverResponse(t.getMessage)
    NotFound()
  }

  private def logWhatIsHiddenForGetOrRecoverResponse(message: String): Unit = {
    logger.info(s"user[$getUserId] ${ request.getMethod } ${ request.getRequestURL } : ${ message }")
  }

  private def depositCreatedResponse(depositInfo: DepositInfo) = {
    Created(
      body = toJson(depositInfo),
      headers = Map("Location" -> s"${ request.getRequestURL }/${ depositInfo.id }")
    )
  }

  private def getUserId: Try[String] = {
    userOption match {
      case Some(u) => Success(u.id)
      case None => Failure(new Exception("No user in a protected servlet. This should be impossible."))
    }
  }

  private def getUUID: Try[UUID] = Try {
    UUID.fromString(params("uuid"))
  }.recoverWith { case t: Throwable =>
    Failure(InvalidResourceException(s"Invalid deposit id: ${ t.getMessage }"))
  }

  private def getPath: Try[Path] = Try {
    Paths.get(multiParams("splat").find(!_.trim.isEmpty).getOrElse(""))
  }.recoverWith { case t: Throwable =>
    logWhatIsHiddenForGetOrRecoverResponse(s"bad path:$t")
    Failure(InvalidResourceException(s"Invalid path."))
  }

  private def getFileItems: Iterable[FileItem] = {
    val fileItems = fileMultiParams.values.flatten
    logger.info(fileItems
      .map(i => s"size=${ i.size } charset=${ i.charset } contentType=${ i.contentType } fieldName=${ i.fieldName } name=${ i.name }")
      .mkString(s"user=${ user.id }; ${ request.uri.getPath }: ", "; ", ".")
    )
    fileItems
  }

  private def isMultipart = {
    val multiPart = "multipart/form-data"
    request.getHeader("Content-Type").blankOption match {
      case Some(s) if s.toLowerCase.startsWith(multiPart) => Success(())
      case x => Failure(new NotImplementedException(s"Expecting Content-Type[$multiPart], got $x."))
    }
  }

  private def getRequestBodyAsManagedInputStream = {
    Try { managed(request.getInputStream) }
  }
}

object DepositServlet {

  private case class InvalidResourceException(s: String) extends Exception(s)
  case class BadRequestException(s: String) extends Exception(s)
}
