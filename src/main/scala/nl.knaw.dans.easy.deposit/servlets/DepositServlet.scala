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
import java.nio.file.{ Path, Paths }
import java.util.UUID

import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.easy.deposit.components.Json.{ InvalidDocument, getDatasetMetadata, getStateInfo, toJson }
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.InvalidResource
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, badDocResponse, internalErrorResponse }
import org.scalatra.{ ActionResult, NotFound, Ok }

import scala.util.{ Failure, Try }

class DepositServlet(app: EasyDepositApiApp) extends ProtectedServlet(app) {

  get("/") {
    forUser(app.getDeposits)
      .map(deposits => Ok(body = toJson(deposits)))
      .getOrRecoverResponse(respond)
  }
  post("/") {
    forUser(app.createDeposit)
      .map(uuid => Ok(
        body = uuid, // TODO UUID will become DepositInfo, which should be wrapped by toJson
        headers = Map("Location" -> s"${ request.getRequestURL }/$uuid")
      ))
      .getOrRecoverResponse(respond)
  }
  get("/:uuid/metadata") {
    forDeposit(app.getDatasetMetadataForDeposit)
      .map(datasetMetadata => Ok(body = toJson(datasetMetadata)))
      .getOrRecoverResponse(respond)
  }
  put("/:uuid/metadata") {
    (for {
      datasetMetadata <- getDatasetMetadata(request.body)
      _ <- forDeposit(app.writeDataMetadataToDeposit(datasetMetadata))
    } yield Ok(???))
      .getOrRecoverResponse(respond)
  }
  get("/:uuid/state") {
    forDeposit(app.getDepositState)
      .map(depositState => Ok(body = toJson(depositState)))
      .getOrRecoverResponse(respond)
  }
  put("/:uuid/state") {
    (for {
      stateInfo <- getStateInfo(request.body)
      _ <- forDeposit(app.setDepositState(stateInfo))
    } yield Ok(???))
      .getOrRecoverResponse(respond)
  }
  delete("/:uuid") {
    forDeposit(app.deleteDeposit)
      .map(_ => Ok(???))
      .getOrRecoverResponse(respond)
  }
  get("/:uuid/file/*") { //dir and file
    forPath(app.getDepositFiles)
      .map(depositFiles => Ok(body = toJson(depositFiles)))
      .getOrRecoverResponse(respond)
  }
  post("/:uuid/file/*") { //dir
    (for {
      inputStream <- getInputStream
      newFileWasCreated <- forPath(app.writeDepositFile(inputStream))
    } yield Ok(???))
      .getOrRecoverResponse(respond)
  }
  put("/:uuid/file/*") { //file
    (for {
      inputStream <- getInputStream
      newFileWasCreated <- forPath(app.writeDepositFile(inputStream))
    } yield Ok(???))
      .getOrRecoverResponse(respond)
  }
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
    // TODO case Failure(t: ???) =>
    case _: InvalidResource =>
      logger.error(s"InvalidResource: ${ t.getMessage }")
      NotFound()
    case _: InvalidDocument => badDocResponse(t)
    case _ => internalErrorResponse(t)
  }

  private def getUUID: Try[UUID] = Try {
    UUID.fromString(params("uuid"))
  }.recoverWith { case t: Throwable =>
    Failure(new InvalidResource(s"Invalid deposit id: ${ t.getMessage }"))
  }

  private def getPath: Try[Path] = Try {
    Paths.get(multiParams("splat").find(!_.trim.isEmpty).getOrElse(""))
  }.recoverWith { case t: Throwable =>
    logger.error(s"bad path:${ t.getClass.getName } ${ t.getMessage }")
    Failure(new InvalidResource(s"Invalid path."))
  }

  private def getInputStream: Try[InputStream] = ???
}

object DepositServlet {

  private class InvalidResource(s: String) extends Exception(s)
}
