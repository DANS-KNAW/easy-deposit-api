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

import java.io.IOException
import java.nio.file.{ InvalidPathException, Path, Paths }
import java.util.UUID

import javax.servlet.ServletInputStream
import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.easy.deposit.Errors._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, StateInfo }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.servlet._
import org.apache.commons.lang.NotImplementedException
import org.scalatra._
import org.scalatra.servlet.{ FileUploadSupport, MultipartConfig, SizeConstraintExceededException }
import org.scalatra.util.RicherString._
import resource.{ ManagedResource, managed }

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp)
  extends ProtectedServlet(app)
    with FileUploadSupport {
  configureMultipartHandling(MultipartConfig())
  error {
    case e: SizeConstraintExceededException => RequestEntityTooLarge(s"too much! ${ e.getMessage }")
    case e: IOException => s"MultipartHandling Exception: $e"
  }
  /*
   * Defensive programming convention at this top level, everything in a for-comprehension:
   *
   *    (for {...} yield ...).getOrRecover(respond).logResponse
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
    {
      for {
        userId <- getUserId
        deposits <- app.getDeposits(userId)
      } yield Ok(body = toJson(deposits), headers = Map(contentTypeJson))
    }.getOrRecover(respond)
      .logResponse
  }
  post("/") {
    {
      for {
        userId <- getUserId
        depositInfo <- app.createDeposit(userId)
        locationHeader = "Location" -> s"${ request.getRequestURL }/${ depositInfo.id }"
      } yield Created(body = toJson(depositInfo), headers = Map(contentTypeJson, locationHeader))
    }.getOrRecover(respond)
      .logResponse
  }
  get("/:uuid/metadata") {
    {
      for {
        uuid <- getUUID
        dmd <- app.getDatasetMetadataForDeposit(user.id, uuid)
      } yield Ok(body = toJson(dmd), headers = Map(contentTypeJson))
    }.getOrRecover(respond)
      .logResponse
  }
  get("/:uuid/doi") {
    {
      for {
        uuid <- getUUID
        doi <- app.getDoi(user.id, uuid)
      } yield Ok(body = s"""{"doi":"$doi"}""", headers = Map(contentTypeJson))
    }.getOrRecover(respond)
      .logResponse
  }
  put("/:uuid/metadata") {
    {
      for {
        uuid <- getUUID
        managedIS = getRequestBodyAsManagedInputStream
        datasetMetadata <- managedIS.apply(is => DatasetMetadata(is))
        _ <- app.checkDoi(user.id, uuid, datasetMetadata)
        _ <- app.writeDataMetadataToDeposit(datasetMetadata, user.id, uuid)
      } yield NoContent()
    }.getOrRecover(respond)
      .logResponse
  }
  get("/:uuid/state") {
    {
      for {
        uuid <- getUUID
        depositState <- app.getDepositState(user.id, uuid)
      } yield Ok(body = toJson(depositState), headers = Map(contentTypeJson))
    }.getOrRecover(respond)
      .logResponse
  }
  put("/:uuid/state") {
    {
      for {
        uuid <- getUUID
        managedIS = getRequestBodyAsManagedInputStream
        stateInfo <- managedIS.apply(is => StateInfo(is))
        _ <- app.setDepositState(stateInfo, user.id, uuid)
      } yield NoContent()
    }.getOrRecover(respond)
      .logResponse
  }
  delete("/:uuid") {
    {
      for {
        uuid <- getUUID
        _ <- app.deleteDeposit(user.id, uuid)
      } yield NoContent()
    }.getOrRecover(respond)
      .logResponse
  }
  get("/:uuid/file/*") { //dir and file
    {
      for {
        uuid <- getUUID
        path <- getPath
        contents <- app.getFileInfo(user.id, uuid, path)
      } yield Ok(body = toJson(contents), headers = Map(contentTypeJson))
    }.getOrRecover(respond)
      .logResponse
  }
  post("/:uuid/file/*") { //file(s)
    {
      for {
        uuid <- getUUID
        path <- getPath
        _ <- isMultipart
        fileItems = fileMultiParams.valuesIterator.flatten.buffered
        maybeZipInputStream <- fileItems.nextAsZipIfOnlyOne
        (managedStagingDir, stagedFilesTarget) <- app.stageFiles(user.id, uuid, path)
        _ <- managedStagingDir.apply(stagingDir =>
          maybeZipInputStream
            .map(_.unzipPlainEntriesTo(stagingDir))
            .getOrElse(fileItems.copyPlainItemsTo(stagingDir))
            .flatMap(_ => stagedFilesTarget.moveAllFrom(stagingDir))
        )
      } yield Created()
    }.getOrRecover(respond)
      .logResponse
  }

  put("/:uuid/file/*") { //file
    {
      for {
        uuid <- getUUID
        path <- getPath
        managedIS = getRequestBodyAsManagedInputStream
        newFileWasCreated <- managedIS.apply(app.writeDepositFile(_, user.id, uuid, path, Option(request.getContentType)))
      } yield if (newFileWasCreated)
                Created(headers = Map("Location" -> request.uri.toASCIIString))
              else NoContent()
    }.getOrRecover(respond)
      .logResponse
  }
  delete("/:uuid/file/*") { //dir and file
    {
      for {
        uuid <- getUUID
        path <- getPath
        _ <- app.deleteDepositFile(user.id, uuid, path)
      } yield NoContent()
    }.getOrRecover(respond)
      .logResponse
  }

  private def respond(t: Throwable): ActionResult = t match {
    case e: ForbiddenException => Forbidden(e.getMessage, Map(contentTypePlainText))
    case e: NotFoundException => NotFound(e.getMessage, Map(contentTypePlainText))
    case e: ConflictException => Conflict(e.getMessage, Map(contentTypePlainText))
    case e: BadRequestException => BadRequest(e.getMessage, Map(contentTypePlainText))
    case e: NotImplementedException => NotImplemented(e.getMessage, Map(contentTypePlainText))
    case _ => notExpectedExceptionResponse(t)
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
  }.recoverWith { // invalid characters, or other file system specific reasons.
    case t: InvalidPathException => Failure(InvalidResourceException(s"Invalid path: ${ t.getMessage }"))
  }

  private def isMultipart: Try[Unit] = {
    val multiPart = "multipart/"
    request.getHeader("Content-Type").blankOption match {
      case Some(s) if s.toLowerCase.startsWith(multiPart) => Success(())
      case x => Failure(InvalidContentTypeException(x, s"""must start with "$multiPart"."""))
    }
  }

  private def getRequestBodyAsManagedInputStream: ManagedResource[ServletInputStream] = managed(request.getInputStream)
}
