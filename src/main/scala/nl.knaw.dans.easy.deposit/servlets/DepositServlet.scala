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

import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.easy.deposit.Errors._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, StateInfo }
import nl.knaw.dans.lib.string._
import org.scalatra._
import org.scalatra.servlet.{ FileUploadSupport, SizeConstraintExceededException }
import org.scalatra.util.RicherString._
import resource.managed

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp)
  extends ProtectedServlet(app)
    with FileUploadSupport {

  configureMultipartHandling(app.multipartConfig)

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
        _ = logger.info(s"retrieve deposits for user '$userId'")
        deposits <- app.getDeposits(userId)
      } yield Ok(body = toJson(deposits), headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
  }
  post("/") {
    {
      for {
        userId <- getUserId
        _ = logger.info(s"creating deposit for user '$userId'")
        depositInfo <- app.createDeposit(userId)
        _ = logger.info(s"[${ depositInfo.id }] created deposit for user '$userId'")
        locationHeader = "Location" -> s"${ request.getRequestURL }/${ depositInfo.id }"
      } yield Created(body = toJson(depositInfo), headers = Map(contentTypeJson, locationHeader))
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/metadata") {
    {
      for {
        uuid <- getUUID
        _ = logger.info(s"[$uuid] retrieve metadata")
        dmd <- app.getDatasetMetadataForDeposit(user.id, uuid)
      } yield Ok(body = toJson(dmd), headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/doi") {
    {
      for {
        uuid <- getUUID
        _ = logger.info(s"[$uuid] retrieve doi")
        doi <- app.getDoi(user.id, uuid)
      } yield Ok(body = s"""{"doi":"$doi"}""", headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
  }
  put("/:uuid/metadata") {
    {
      for {
        uuid <- getUUID
        _ = logger.info(s"[$uuid] saving metadata")
        managedIS = managed(request.getInputStream)
        datasetMetadata <- managedIS.apply(is => DatasetMetadata(is))
        _ = logger.debug(s"[$uuid] comparing DOI in deposit.properties and newly uploaded dataset metadata")
        _ <- app.checkDoi(user.id, uuid, datasetMetadata)
        _ = logger.debug(s"[$uuid] writing newly uploaded dataset metadata to deposit")
        _ <- app.writeDataMetadataToDeposit(datasetMetadata, user.id, uuid)
      } yield NoContent()
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/state") {
    {
      for {
        uuid <- getUUID
        _ = logger.info(s"[$uuid] retrieve deposit state")
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
        _ = logger.info(s"[$uuid] changing state to ${ stateInfo.state } with description ${ stateInfo.stateDescription }")
        _ <- app.setDepositState(stateInfo, user.id, uuid)
      } yield NoContent()
    }.getOrRecoverWithActionResult
  }
  delete("/:uuid") {
    {
      for {
        uuid <- getUUID
        _ = logger.info(s"[$uuid] deleting deposit")
        _ <- app.deleteDeposit(user.id, uuid)
      } yield NoContent()
    }.getOrRecoverWithActionResult
  }
  get("/:uuid/file/*") { //dir and file
    {
      for {
        uuid <- getUUID
        path <- getRelativeLocation(s"[$uuid] retrieve file info for path")
        contents <- app.getFileInfo(user.id, uuid, path)
      } yield Ok(body = toJson(contents), headers = Map(contentTypeJson))
    }.getOrRecoverWithActionResult
  }
  post("/:uuid/file/*") { //file(s)
    {
      for {
        uuid <- getUUID
        path <- getRelativeLocation(s"[$uuid] upload files to path") // plural
        _ <- isMultipart
        fileItems = fileMultiParams.toMap.valuesIterator.flatten.buffered
        maybeManagedArchiveInputStream <- fileItems.nextAsArchiveIfOnlyOne
        (managedStagingDir, draftDataFiles) <- app.stagingContext(user.id, uuid)
        _ <- managedStagingDir.apply(stagingDir =>
          maybeManagedArchiveInputStream
            .map(_.unpackPlainEntriesTo(stagingDir, uuid))
            .getOrElse(fileItems.moveNonArchive(app.multipartConfig.location, stagingDir, uuid))
            .flatMap(_ => draftDataFiles.moveAll(stagingDir, path))
        )
      } yield Created()
    }.getOrRecoverWithActionResult
  }

  put("/:uuid/file/*") { //file
    {
      for {
        uuid <- getUUID
        path <- getRelativeLocation(s"[$uuid] upload file to path") // single
        managedIS = managed(request.getInputStream)
        newFileWasCreated <- managedIS.apply(app.writeDepositFile(_, user.id, uuid, path, Option(request.getContentType)))
        _ = logger.info(s"[$uuid] ${
          if (newFileWasCreated) "no "
          else ""
        }new file was created")
      } yield if (newFileWasCreated)
                Created(headers = Map("Location" -> request.uri.toASCIIString))
              else NoContent()
    }.getOrRecoverWithActionResult
  }
  delete("/:uuid/file/*") { //dir and file
    {
      for {
        uuid <- getUUID
        path <- getRelativeLocation(s"[$uuid] deleting file")
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

  private def getUUID: Try[UUID] = {
    params("uuid").toUUID.toTry
      .recoverWith { case e => Failure(InvalidResourceException(s"Invalid deposit id: ${ e.getMessage }")) }
  }

  private def getRelativeLocation(logPrefix: String): Try[Path] = Try {
    val splat = multiParams("splat")
      .find(!_.trim.isEmpty)
      .getOrElse("")
    logger.info(s"$logPrefix '${ splat.toOption.getOrElse("/") }'")
    Paths.get(splat)
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
