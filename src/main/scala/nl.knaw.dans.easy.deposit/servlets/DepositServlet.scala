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

import java.nio.file.{ Path, Paths }
import java.util.UUID

import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.easy.deposit.docs.Json.{ InvalidDocumentException, getDatasetMetadata, getStateInfo, toJson }
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.InvalidResourceException
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import org.scalatra._
import resource.managed

import scala.util.{ Failure, Try }

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
  put("/:uuid/metadata") {
    {
      for {
        managedIS <- Try { managed(request.getInputStream) }
        datasetMetadata <- managedIS.apply(is => getDatasetMetadata(is))
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
        managedIS <- Try { managed(request.getInputStream) }
        stateInfo <- managedIS.apply(is => getStateInfo(is))
        _ <- forDeposit(app.setDepositState(stateInfo))
      } yield Ok(???)
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
  post("/:uuid/file/*") { upload } //dir
  put("/:uuid/file/*") { upload } //file
  private def upload = {
    for {
      managedIS <- Try { managed(request.getInputStream) }
      newFileWasCreated <- managedIS.apply(is => forPath(app.writeDepositFile(is)))
    } yield if (newFileWasCreated)
              Created(headers = Map("Location" -> request.uri.toASCIIString))
            else Ok()
  }.getOrRecoverResponse(respond)

  delete("/:uuid/file/*") { //dir and file
    forPath(app.deleteDepositFile)
      .map(_ => Ok(???))
      .getOrRecoverResponse(respond)
  }

  private def forUser[T](callback: (String) => Try[T]
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

  private def respond(t: Throwable): ActionResult = t match {
    case _: IllegalStateTransitionException => ???
    case e: NoSuchDepositException => NoSuchDespositResponse(e)
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
    Failure(new InvalidResourceException(s"Invalid deposit id: ${ t.getMessage }"))
  }

  private def getPath: Try[Path] = Try {
    Paths.get(multiParams("splat").find(!_.trim.isEmpty).getOrElse(""))
  }.recoverWith { case t: Throwable =>
    logger.error(s"bad path:${ t.getClass.getName } ${ t.getMessage }")
    Failure(new InvalidResourceException(s"Invalid path."))
  }
}

object DepositServlet {

  private class InvalidResourceException(s: String) extends Exception(s)
}
