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

import better.files.File
import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.easy.deposit.Errors._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, StateInfo }
import org.scalatra._
import org.scalatra.servlet.{ FileUploadSupport, SizeConstraintExceededException }
import org.scalatra.util.RicherString._
import resource.managed

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp)
  extends ProtectedServlet(app)
    with FileUploadSupport {

  configureMultipartHandling(app.multipartConfig.copy(location = app.multipartConfig.location
    .filter(_.trim.nonEmpty)
    .map { s =>
      val dir = File(s)
      val absolutePath = dir.path.toAbsolutePath
      if (!dir.isDirectory && !dir.isReadable && !dir.isWriteable)
        throw ConfigurationException(s"$absolutePath not found/readable/writable or not a directory")
      absolutePath.toString
    }
  ))

  error {
    case e: SizeConstraintExceededException => RequestEntityTooLarge(s"too much! ${ e.getMessage }")
    case e: IOException => s"MultipartHandling Exception: $e"
  }
  /*
   * Defensive programming convention at this top level, everything in a for-comprehension:
   *
   *    (for {...} yield ...).getOrRecoverWithActionResult.logResponse
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
    }.getOrRecoverWithActionResult
  }
  post("/") {
    {
      for {
        userId <- getUserId
        depositInfo <- app.createDeposit(userId)
        locationHeader = "Location" -> s"${ request.getRequestURL }/${ depositInfo.id }"
      } yield Created(body = toJson(depositInfo), headers = Map(contentTypeJson, locationHeader))
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/metadata") {
    {
      for {
        uuid <- getUUID
        dmd <- app.getDatasetMetadataForDeposit(user.id, uuid)
      } yield Ok(body = toJson(dmd), headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/doi") {
    {
      for {
        uuid <- getUUID
        doi <- app.getDoi(user.id, uuid)
      } yield Ok(body = s"""{"doi":"$doi"}""", headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
  }
  put("/:uuid/metadata") {
    {
      for {
        uuid <- getUUID
        managedIS = managed(request.getInputStream)
        datasetMetadata <- managedIS.apply(is => DatasetMetadata(is))
        _ <- app.checkDoi(user.id, uuid, datasetMetadata)
        _ <- app.writeDataMetadataToDeposit(datasetMetadata, user.id, uuid)
      } yield NoContent()
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/state") {
    {
      for {
        uuid <- getUUID
        depositState <- app.getDepositState(user.id, uuid)
      } yield Ok(body = toJson(depositState), headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
  }
  put("/:uuid/state") {
    {
      for {
        uuid <- getUUID
        managedIS = managed(request.getInputStream)
        stateInfo <- managedIS.apply(is => StateInfo(is))
        _ <- app.setDepositState(stateInfo, user.id, uuid)
      } yield NoContent()
    }.getOrRecoverWithActionResult
  }
  delete("/:uuid") {
    {
      for {
        uuid <- getUUID
        _ <- app.deleteDeposit(user.id, uuid)
      } yield NoContent()
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/file/*") { //dir and file
    {
      for {
        uuid <- getUUID
        path <- getPath
        contents <- app.getFileInfo(user.id, uuid, path)
      } yield Ok(body = toJson(contents), headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
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
            .getOrElse(app.multipartConfig.moveNonZips(fileItems, stagingDir))
            .flatMap(_ => stagedFilesTarget.moveAllFrom(stagingDir))
        )
      } yield Created()
    }.getOrRecoverWithActionResult
  }

  put("/:uuid/file/*") { //file
    {
      for {
        uuid <- getUUID
        path <- getPath
        managedIS = managed(request.getInputStream)
        newFileWasCreated <- managedIS.apply(app.writeDepositFile(_, user.id, uuid, path, Option(request.getContentType)))
      } yield if (newFileWasCreated)
                Created(headers = Map("Location" -> request.uri.toASCIIString))
              else NoContent()
    }.getOrRecoverWithActionResult
  }
  delete("/:uuid/file/*") { //dir and file
    {
      for {
        uuid <- getUUID
        path <- getPath
        _ <- app.deleteDepositFile(user.id, uuid, path)
      } yield NoContent()
    }.getOrRecoverWithActionResult
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
}
