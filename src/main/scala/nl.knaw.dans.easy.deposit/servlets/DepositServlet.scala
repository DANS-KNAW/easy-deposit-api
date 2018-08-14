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

import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, toJson }
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, StateInfo }
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.{ BadRequestException, InvalidResourceException }
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import org.scalatra._
import resource.managed

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp) extends ProtectedServlet(app) {

  get("/") {
    forUser(app.getDeposits)
      .map(deposits => Ok(body = toJson(deposits)))
      .getOrRecoverResponse(respond)
  }
  post("/") {
    forUser(app.createDeposit).map { depositInfo =>
      Created(
        body = toJson(depositInfo),
        headers = Map("Location" -> s"${ request.getRequestURL }/${ depositInfo.id }")
      )
    }.getOrRecoverResponse(respond)
  }
  get("/:uuid/metadata") {
    forDeposit(app.getDatasetMetadataForDeposit)
      .map(datasetMetadata => Ok(body = toJson(datasetMetadata)))
      .getOrRecoverResponse(respond)
  }
  get("/:uuid/doi") {
    forDeposit(app.getDoi)
      .map(doi => Ok(body = s"""{"doi":"$doi"}"""))
      .getOrRecoverResponse(respond)
  }
  put("/:uuid/metadata") {
    {
      for {
        managedIS <- getRequestBodyAsManagedInputStream
        datasetMetadata <- managedIS.apply(is => DatasetMetadata(is))
        _ <- forDeposit(app.writeDataMetadataToDeposit(datasetMetadata))
      } yield NoContent()
    }.getOrRecoverResponse(respond)
  }
  get("/:uuid/state") {
    forDeposit(app.getDepositState)
      .map(depositState => Ok(body = toJson(depositState)))
      .getOrRecoverResponse(respond)
  }
  put("/:uuid/state") {
    {
      for {
        managedIS <- getRequestBodyAsManagedInputStream
        stateInfo <- managedIS.apply(is => StateInfo(is))
        _ <- forDeposit(app.setDepositState(stateInfo))
      } yield NoContent()
    }.getOrRecoverResponse(respond)
  }

  delete("/:uuid") {
    forDeposit(app.deleteDeposit)
      .map(_ => NoContent())
      .getOrRecoverResponse(respond)
  }
  get("/:uuid/file/*") { //dir and file
    forPath(app.getDepositFiles)
      .map(depositFiles => Ok(body = toJson(depositFiles)))
      .getOrRecoverResponse(respond)
  }
  post("/:uuid/file/*") { //file
    {
      for { // a unique request signature so no forXxx method
        managedIS <- getRequestBodyAsManagedInputStream
        uuid <- getUUID
        dir <- getPath
        // TODO EASY-1658: mime type application/zip or application/binary
        disposition <- getMandatoryHeader("Content-Disposition")
        path = dir.resolve(disposition)
        newFileWasCreated <- managedIS.apply(is => app.writeDepositFile(is)(user.id, uuid, path))
      } yield newFileHeader(newFileWasCreated)
    }.getOrRecoverResponse(respond)
  }
  put("/:uuid/file/*") { //file
    {
      for {
        managedIS <- getRequestBodyAsManagedInputStream
        newFileWasCreated <- managedIS.apply(is => forPath(app.writeDepositFile(is)))
      } yield newFileHeader(newFileWasCreated)
    }.getOrRecoverResponse(respond)
  }
  delete("/:uuid/file/*") { //dir and file
    forPath(app.deleteDepositFile)
      .map(_ => NoContent())
      .getOrRecoverResponse(respond)
  }

  private def forUser[T](callback: String => Try[T]
                        ): Try[T] = {
    // shortest callBack signature
    // the signatures reflect the parameters in the route patterns and authenticated user
    Try(callback(user.id)).flatten // catch throws that slipped through
  }

  private def forDeposit[T](callback: (String, UUID) => Try[T]
                           ): Try[T] = {
    for {
      uuid <- getUUID
      result <- Try(callback(user.id, uuid)).flatten // catch throws that slipped through
    } yield result
  }

  private def forPath[T](callback: (String, UUID, Path) => Try[T]
                        ): Try[T] = {
    for {
      uuid <- getUUID
      path <- getPath
      result <- Try(callback(user.id, uuid, path)).flatten // catch throws that slipped through
    } yield result
  }

  private def newFileHeader(newFileWasCreated: Boolean) = {
    if (newFileWasCreated)
      Created(headers = Map("Location" -> request.uri.toASCIIString))
    else Ok()
  }

  private def respond(t: Throwable): ActionResult = t match {
    case e: IllegalStateTransitionException => Forbidden(e.getMessage)
    case e: NoSuchDepositException => NoSuchDespositResponse(e)
    case e: NoSuchFileException => NotFound(body = s"${ e.getMessage } not found")
    case e: InvalidResourceException => InvalidResourceResponse(e)
    case e: InvalidDocumentException => badDocResponse(e)
    case _ => internalErrorResponse(t)
  }

  private def NoSuchDespositResponse(e: NoSuchDepositException) = {
    // we log but don't expose which file was not found
    logger.info(e.getMessage)
    NotFound(body = s"Deposit ${ e.id } not found")
  }

  private def InvalidResourceResponse(t: InvalidResourceException) = {
    // we log but don't expose which part of the uri was invalid
    logger.error(s"${ t.getMessage }")
    NotFound()
  }

  private def getUUID: Try[UUID] = Try {
    UUID.fromString(params("uuid"))
  }.recoverWith { case t: Throwable =>
    Failure(InvalidResourceException(s"Invalid deposit id: ${ t.getMessage }"))
  }

  private def getPath: Try[Path] = Try {
    Paths.get(multiParams("splat").find(!_.trim.isEmpty).getOrElse(""))
  }.recoverWith { case t: Throwable =>
    logger.error(s"bad path:${ t.getClass.getName } ${ t.getMessage }")
    Failure(InvalidResourceException(s"Invalid path."))
  }

  private def getMandatoryHeader(headerName: String): Try[String] = {
    Some(request.getHeader(headerName))
      .find(!_.trim.isEmpty)
    match {
      case Some(s) => Success(s)
      case None => Failure(BadRequestException(s"No $headerName header found"))
    }
  }

  private def getRequestBodyAsManagedInputStream = {
    Try { managed(request.getInputStream) }
  }
}

object DepositServlet {

  private case class InvalidResourceException(s: String) extends Exception(s)
  private case class BadRequestException(s: String) extends Exception(s)
}
