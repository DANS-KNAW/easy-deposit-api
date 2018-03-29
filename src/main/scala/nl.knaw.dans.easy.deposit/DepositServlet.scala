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
import org.json4s.JsonAST.{ JNull, JString }
import org.json4s.ext.{ EnumNameSerializer, JodaTimeSerializers, UUIDSerializer }
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization.write
import org.json4s.{ CustomSerializer, DefaultFormats, Formats, JsonInput }
import org.scalatra._

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp) extends AbstractAuthServlet(app) {

  before() {
    if (!isAuthenticated) {
      halt(Forbidden("missing, invalid or expired credentials").logResponse)
    }
  }

  get("/user") {
    forUser(app.getUser)
      .map(map => Ok(toJson(User(map))))
      .getOrRecover(respond)
  }

  get("/") {
    forUser(app.getDeposits)
      .map(deposits => Ok(body = toJson(deposits)))
      .getOrRecover(respond)
  }
  post("/") {
    forUser(app.createDeposit)
      .map(uuid => Ok(
        body = uuid, // TODO UUID will become DepositInfo, which should be wrapped by toJson
        headers = Map("Location" -> s"${ request.getRequestURL }/$uuid")
      ))
      .getOrRecover(respond)
  }
  get("/:uuid/metadata") {
    forDeposit(app.getDatasetMetadataForDeposit)
      .map(datasetMetadata => Ok(body = toJson(datasetMetadata)))
      .getOrRecover(respond)
  }
  put("/:uuid/metadata") {
    (for {
      datasetMetadata <- getDatasetMetadata(request.body)
      _ <- forDeposit(app.writeDataMetadataToDeposit(datasetMetadata))
    } yield Ok(???))
      .getOrRecover(respond)
  }
  get("/:uuid/state") {
    forDeposit(app.getDepositState)
      .map(depositState => Ok(body = toJson(depositState)))
      .getOrRecover(respond)
  }
  put("/:uuid/state") {
    (for {
      stateInfo <- getStateInfo(request.body)
      _ <- forDeposit(app.setDepositState(stateInfo))
    } yield Ok(???))
      .getOrRecover(respond)
  }
  delete("/:uuid") {
    forDeposit(app.deleteDeposit)
      .map(_ => Ok(???))
      .getOrRecover(respond)
  }
  get("/:uuid/file/*") { //dir and file
    forPath(app.getDepositFiles)
      .map(depositFiles => Ok(body = toJson(depositFiles)))
      .getOrRecover(respond)
  }
  post("/:uuid/file/*") { //dir
    (for {
      inputStream <- getInputStream
      newFileWasCreated <- forPath(app.writeDepositFile(inputStream))
    } yield Ok(???))
      .getOrRecover(respond)
  }
  put("/:uuid/file/*") { //file
    (for {
      inputStream <- getInputStream
      newFileWasCreated <- forPath(app.writeDepositFile(inputStream))
    } yield Ok(???))
      .getOrRecover(respond)
  }
  delete("/:uuid/file/*") { //dir and file
    forPath(app.deleteDepositFile)
      .map(_ => Ok(???))
      .getOrRecover(respond)
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

  private def respond(t: Throwable): ActionResult = {
    t match {
      // TODO case Failure(t: ???) =>
      case _: InvalidResource =>
        logger.error(s"InvalidResource: ${ t.getMessage }")
        NotFound()
      case _: InvalidDocument =>
        logger.error(s"Invalid ${ t.getMessage }:${ t.getCause.getClass.getName } ${ t.getCause.getMessage }")
        BadRequest(s"Bad Request. The ${ t.getMessage } document is malformed.")
      case _ =>
        logger.error(s"Not expected exception: ${ t.getMessage }", t)
        InternalServerError("Internal Server Error")
    }
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
  private class InvalidDocument(s: String, t: Throwable) extends Exception(s, t)

  class PathSerializer extends CustomSerializer[Path](format =>
    ( {
      case JString(s) => Paths.get(s)
      case JNull => null
    }, {
      case x: Path => JString(x.toString)
    }
    )
  )

  private implicit val jsonFormats: Formats = new DefaultFormats {
    // TODO we need a timestamp for DepositInfo, but may need dates for not yet implemented members of DatasetMetadata
    override protected def dateFormatter: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
  } +
    UUIDSerializer +
    new PathSerializer +
    new EnumNameSerializer(State) ++
    JodaTimeSerializers.all

  private def toJson[A <: AnyRef](a: A): String = {
    // seems not to need a try: while the date formatter wasn't in place it produced empty strings
    write(a)
  }

  private def getStateInfo(body: JsonInput): Try[StateInfo] = Try {
    JsonMethods.parse(body).extract[StateInfo]
  }.recoverWith { case t: Throwable => Failure(new InvalidDocument("StateInfo", t)) }

  private def getDatasetMetadata(body: JsonInput): Try[DatasetMetadata] = Try {
    JsonMethods.parse(body).extract[DatasetMetadata]
  }.recoverWith { case t: Throwable => Failure(new InvalidDocument("DatasetMetadata", t)) }
}
