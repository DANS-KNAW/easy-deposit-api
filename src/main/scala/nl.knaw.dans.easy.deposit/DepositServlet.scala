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
package nl.knaw.dans.easy.deposit

import java.io.InputStream
import java.nio.file.{ Path, Paths }
import java.text.SimpleDateFormat
import java.util.UUID

import nl.knaw.dans.easy.deposit.DepositServlet._
import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.lib.error._
import org.json4s.ext.{ EnumNameSerializer, JodaTimeSerializers }
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization.write
import org.json4s.{ DefaultFormats, Formats }
import org.scalatra._

import scala.util.{ Failure, Try }

class DepositServlet(app: EasyDepositApiApp) extends AbstractAuthServlet(app) {

  before() {
    if (!isAuthenticated) {
      halt(Forbidden("missing, invalid or expired credentials").logResponse())
    }
  }

  get("/") {
    recoverIfFailureAndLog(
      forUser(app.getDeposits)
        .map(deposits => Ok(body = toJson(deposits)))
    )
  }
  post("/") {
    recoverIfFailureAndLog(
      forUser(app.createDeposit)
        .map(uuid => Ok(
          body = uuid, // TODO UUID will become DepositInfo, which should be wrapped by toJson
          headers = Map("Location" -> s"${ request.getRequestURL }/$uuid")
        ))
    )
  }
  get("/:id/metadata") {
    recoverIfFailureAndLog(
      forDeposit(app.getDatasetMetadataForDeposit)
        .map(datasetMetadata => Ok(body = toJson(datasetMetadata)))
    )
  }
  put("/:id/metadata") {
    recoverIfFailureAndLog(for {
      datasetMetadata <- getDatasetMetadata
      _ <- forDeposit(app.writeDataMetadataToDeposit(datasetMetadata))
    } yield Ok(???))
  }
  get("/:id/state") {
    recoverIfFailureAndLog(
      forDeposit(app.getDepositState)
        .map(depositState => Ok(body = toJson(depositState)))
    )
  }
  put("/:id/state") {
    recoverIfFailureAndLog(for {
      stateInfo <- getStateInfo
      _ <- forDeposit(app.setDepositState(stateInfo))
    } yield Ok(???))
  }
  delete("/:id") {
    recoverIfFailureAndLog(
      forDeposit(app.deleteDeposit)
        .map(_ => Ok(???))
    )
  }
  get("/:id/file/*") { //dir and file
    recoverIfFailureAndLog(
      forPath(app.getDepositFiles)
        .map(depositFiles => Ok(body = toJson(depositFiles)))
    )
  }
  post("/:id/file/*") { //dir
    recoverIfFailureAndLog(for {
      inputStream <- getInputStream
      newFileWasCreated <- forPath(app.writeDepositFile(inputStream))
    } yield Ok(???))
  }
  put("/:id/file/*") { //file
    recoverIfFailureAndLog(for {
      inputStream <- getInputStream
      newFileWasCreated <- forPath(app.writeDepositFile(inputStream))
    } yield Ok(???))

  }
  delete("/:id/file/*") { //dir and file
    recoverIfFailureAndLog(
      forPath(app.deleteDepositFile)
        .map(_ => Ok(???))
    )
  }

  private def forUser[T](callback: (String) => Try[T]): Try[T] = {
    // shortest callBack signature of the forXxx(callBack) methods
    // the signatures reflect the parameters in the route patterns and authenticated user
    Try(callback(user.id)).flatten // catch throws that slipped through
  }

  private def forDeposit[T](callback: (String, UUID) => Try[T]): Try[T] = {
    for {
      uuid <- getUUID
      result <- Try(callback(user.id, uuid)).flatten // catch throws that slipped through
    } yield result
  }

  private def forPath[Result](callback: (String, UUID, Path) => Try[Result]): Try[Result] = {
    for {
      uuid <- getUUID
      path <- getPath
      result <- Try(callback(user.id, uuid, path)).flatten // catch throws that slipped through
    } yield result
  }


  private def recoverIfFailureAndLog(appResult: Try[ActionResult]): ActionResult = {
    appResult.getOrRecover {
      // TODO case Failure(t: ???) =>
      case t: InvalidResource =>
        logger.error(s"InvalidResource: ${ t.getMessage }")
        NotFound()
      case t =>
        logger.error(s"Not expected exception: ${ t.getMessage }", t)
        InternalServerError("Internal Server Error")
    }
  }.logResponse() // not pure but prevents repeating and occasionally forgetting

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

  private implicit val jsonFormats: Formats = new DefaultFormats {
    override protected def dateFormatter: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
  } + new EnumNameSerializer(State) ++ JodaTimeSerializers.all


  def toJson[A <: AnyRef](a: A): String = {
    // just for readability
    // seems not to need a try: while the date formatter wasn't in place it produced empty strings
    write(a)
  }

  private def getStateInfo: Try[StateInfo] = Try {
    // TODO verify mime type?
    JsonMethods.parse(request.body).extract[StateInfo]
  }.recoverWith { case t: Throwable =>
    logger.error(s"bad StateInfo:${ t.getClass.getName } ${ t.getMessage }")
    Failure(new Exception(s"Bad Request. The state document is malformed."))
  }

  private def getDatasetMetadata: Try[DatasetMetadata] = Try {
    // TODO verify mime type?
    JsonMethods.parse(request.body).extract[DatasetMetadata]
  }.recoverWith { case t: Throwable =>
    logger.error(s"bad DatasetMetadata:${ t.getClass.getName } ${ t.getMessage }")
    Failure(new Exception(s"Bad Request. The metadata document is malformed."))
  }
}
object DepositServlet {

  private class InvalidResource(s: String) extends Exception(s)
}