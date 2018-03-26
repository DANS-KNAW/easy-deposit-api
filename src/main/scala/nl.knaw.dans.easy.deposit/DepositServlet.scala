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
import java.util.UUID

import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus
import org.json4s.native.JsonMethods
import org.json4s.{ DefaultFormats, Formats }
import org.scalatra._

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp) extends AbstractAuthServlet(app) {

  before() {
    if (!isAuthenticated) {
      halt(HttpStatus.FORBIDDEN_403, "missing, invalid or expired credentials")
    }
  }

  get("/") { forUser(app.getDeposits) }
  post("/") { forUser(app.createDeposit) }
  get("/:id/metadata") { forDeposit(app.getDatasetMetadataForDeposit) }
  put("/:id/metadata") { getDatasetMetadata.map(m => forDeposit(app.writeDataMetadataToDeposit(m))).getOrRecover(badDoc) }
  get("/:id/state") { forDeposit(app.getDepositState) }
  put("/:id/state") { getStateInfo.map(s => forDeposit(app.setDepositState(s))).getOrRecover(badDoc) }
  delete("/:id") { forDeposit(app.deleteDeposit) }
  get("/:id/file/*") { forPath(app.getDepositFiles) } //dir and file
  post("/:id/file/*") { getInputStream.map(is => forPath(app.writeDepositFile(is))).getOrRecover(badInputStream) } //dir
  put("/:id/file/*") { getInputStream.map(is => forPath(app.writeDepositFile(is))).getOrRecover(badInputStream) } //file
  delete("/:id/file/*") { forPath(app.deleteDepositFile) } //dir and file

  private def forUser[T](callback: (String) => Try[T]): ActionResult = {
    val result = Try(callback(user.id)).flatten // catch throws that slipped through
    respond(result)
  }

  private def forDeposit[T](callback: (String, UUID) => Try[T]): ActionResult = {
    respond(for {
      uuid <- getUUID
      result <- Try(callback(user.id, uuid)).flatten // catch throws that slipped through
    } yield result)
  }

  private def forPath[Result](callback: (String, UUID, Path) => Try[Result]): ActionResult = {
    respond(for {
      uuid <- getUUID
      path <- getPath
      result <- Try(callback(user.id, uuid, path)).flatten // catch throws that slipped through
    } yield result)
  }

  private def respond[Result](appResult: Try[Result]): ActionResult = {
    val actionResult = appResult match {
      case Success(Unit) => Ok() // writeDataMetadataToDeposit, setDepositState, deleteDeposit, deleteDepositFile
      case Success(seq: Seq[_]) if seq.isInstanceOf[DepositInfo] => Ok(???)
      case Success(seq: Seq[_]) if seq.isInstanceOf[FileInfo] => Ok(???)
      case Success(datasetMetadata: DatasetMetadata) => Ok(???)
      case Success(stateInfo: StateInfo) => Ok(???)
      case Success(result: Boolean) => Ok(result.toString) // writeDepositFile
      case Success(uuid: UUID) => Ok( // TODO UUID will change into DepositInfo
        body = uuid.toString,
        headers = Map("Location" -> s"${ request.getRequestURL }/$uuid")
      )
      case Success(x) =>
        logger.error(s"Not expected result type: ${ x.getClass.getName }")
        InternalServerError("Internal Server Error")
      // TODO case Failure(t: ???) =>
      case Failure(t: InvalidResource) =>
        logger.error(s"InvalidResource: ${ t.getMessage }")
        NotFound()
      case Failure(t) =>
        logger.error(s"Not expected exception: ${ t.getMessage }", t)
        InternalServerError("Internal Server Error")
    }
    // TODO remove this workaround when ServiceEnhancedLogging.after is fixed
    // the body in the log might be too much
    logger.info(s"returned status=${ actionResult.status } headers=${ actionResult.headers }")
    actionResult
  }
  private class InvalidResource(s: String) extends Exception(s)

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

  private def badInputStream(t: Throwable): ActionResult = {
    logger.error(s"badInputStream: ${ t.getMessage }", t)
    BadRequest(s"Bad Request.")
  }

  private implicit val jsonFormats: Formats = new DefaultFormats {}

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

  private def badDoc(t: Throwable): ActionResult = {
    BadRequest(t.getMessage)
  }
}
