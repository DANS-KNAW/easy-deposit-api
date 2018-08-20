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

import java.nio.file.{ NoSuchFileException, Path, Paths }
import java.util.UUID

import com.sun.xml.internal.messaging.saaj.packaging.mime.internet.{ ContentDisposition, ParseException }
import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, toJson }
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, StateInfo }
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.{ BadRequestException, InvalidResourceException }
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import org.scalatra._
import resource.managed

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp) extends ProtectedServlet(app) {

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

  post("/:uuid/file/*") { //file
    val zip = "application/zip"
    val bin = "application/octet-stream"
    (for {
      uuid <- getUUID
      path <- getPath
      managedIS <- getRequestBodyAsManagedInputStream // TODO decide when this line upon implementation of zip
      maybeContentType = getLowerCaseHeaderValue("Content-Type")
      mayBeFileName <- filenameFromContentDisposition
      newFileWasCreated <- (maybeContentType, mayBeFileName) match {
        case (Some(`zip`), _) => Failure(???) // TODO issue EASY-1658
        case (Some(`bin`), Some(fileName: String)) =>
          val fullPath = path.resolve(fileName)
          managedIS.apply(is => app.writeDepositFile(is, user.id, uuid, fullPath))
        case (_, _) =>
          val explanation = s"Expecting header 'Content-Type: $zip' or 'Content-Type: $bin'; the latter with a filename in the 'Content-Disposition'."
          Failure(BadRequestException(s"$explanation GOT: $maybeContentType AND $maybeContentType"))
      }
    } yield fileCreatedOrOkResponse(newFileWasCreated)
      ).getOrRecoverResponse(respond)
  }
  put("/:uuid/file/*") { //file
    (for {
      uuid <- getUUID
      path <- getPath
      managedIS <- getRequestBodyAsManagedInputStream
      newFileWasCreated <- managedIS.apply(is => app.writeDepositFile(is, user.id, uuid, path))
    } yield fileCreatedOrOkResponse(newFileWasCreated)
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

  private def respond(t: Throwable): ActionResult = t match {
    case e: IllegalStateTransitionException => Forbidden(e.getMessage)
    case e: NoSuchDepositException => noSuchDepositResponse(e)
    case e: NoSuchFileException => NotFound(body = s"${ e.getMessage } not found")
    case e: InvalidResourceException => invalidResourceResponse(e)
    case e: InvalidDocumentException => badDocResponse(e)
    case e: BadRequestException => BadRequest(e.getMessage)
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

  private def fileCreatedOrOkResponse(newFileWasCreated: Boolean): ActionResult = {
    if (newFileWasCreated)
      Created(headers = Map("Location" -> request.uri.toASCIIString))
    else Ok()
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
    logWhatIsHiddenForGetOrRecoverResponse(s"bad path:${ t.getClass.getName } ${ t.getMessage }")
    Failure(InvalidResourceException(s"Invalid path."))
  }

  private def filenameFromContentDisposition = Try{
    Option(request.getHeader("Content-Disposition"))
      .flatMap(s => Some(
        new ContentDisposition(s)
          .getParameter("filename")
      ))
  }.recoverWith{
    case e: Throwable => Failure(BadRequestException(s"Content-Disposition: ${ e.getMessage }"))
  }

  private def getLowerCaseHeaderValue(headerName: String) = {
    Option(request.getHeader(headerName))
      .find(!_.trim.isEmpty)
      .map(_.toLowerCase)
  }

  private def getRequestBodyAsManagedInputStream = {
    Try { managed(request.getInputStream) }
  }
}

object DepositServlet {

  private case class InvalidResourceException(s: String) extends Exception(s)
  private case class BadRequestException(s: String) extends Exception(s)
}
