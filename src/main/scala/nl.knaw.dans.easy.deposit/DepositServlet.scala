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
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.native.JsonMethods
import org.json4s.{ DefaultFormats, Formats }
import org.scalatra._

import scala.util.{ Failure, Success, Try }

class DepositServlet(app: EasyDepositApiApp) extends ScalatraServlet with DebugEnhancedLogging {

  val userId: String = "user001" // TODO see TestServlet in PR #9
  before() {
    // TODO see TestServlet in PR #9
  }

  get("/") { respond(app.getDeposits(userId)) }
  post("/") { respond(app.createDeposit(userId)) }
  get("/:id/metadata") { forId(app.getDatasetMetadataForDeposit) }
  put("/:id/metadata") { getDatasetMetadata.map(m => forId(app.writeDataMetadataToDeposit(m))).getOrRecover(badDoc) }
  get("/:id/state") { forId(app.getDepositState) }
  put("/:id/state") { getStateInfo.map(s => forId(app.setDepositState(s))).getOrRecover(badDoc) }
  delete("/:id") { forId(app.deleteDeposit) }
  get("/:id/file/*") { forPath(app.getDepositFiles) } //dir and file
  post("/:id/file/*") { getInputStream.map(is => forPath(app.writeDepositFile(is))).getOrRecover(badInputStream) } //dir
  put("/:id/file/*") { getInputStream.map(is => forPath(app.writeDepositFile(is))).getOrRecover(badInputStream) } //file
  delete("/:id/file/*") { forPath(app.deleteDepositFile) } //dir and file

  private def forId[T](callback: (String, UUID) => Try[T]): ActionResult = {
    getUUID match {
      case Failure(t) => BadRequest(t.getMessage)
      case Success(uuid) => respond(callback(userId, uuid))
    }
  }

  private def forPath[Result](callback: (String, UUID, Path) => Try[Result]): ActionResult = {
    (getUUID, getPath) match {
      case (Failure(tId), Failure(tPath)) => BadRequest(s"${ tId.getMessage }. ${ tPath.getMessage }.")
      case (Failure(t), _) => BadRequest(t.getMessage)
      case (_, Failure(t)) => BadRequest(t.getMessage)
      case (Success(uuid), Success(path)) => respond(callback(userId, uuid, path))
    }
  }

  private def respond[Result](result: Try[Result]): ActionResult = {
    result match {
      case Success(Unit) => Ok()
      case Success(seq: Seq[_]) if seq.isInstanceOf[DepositInfo] => Ok(???)
      case Success(datasetMetadata: DatasetMetadata) => Ok(???)
      case Success(stateInfo: StateInfo) => Ok(???)
      case Success(result: Boolean) => Ok(result.toString) // writeDepositFile
      case Success(uuid: UUID) => Ok(uuid.toString)
      case Success(x) =>
        logger.error(s"not expected result type: ${ x.getClass.getName }")
        InternalServerError("Internal Server Error")
      // TODO case Failure(t: ???) =>
      case Failure(t) =>
        logger.error(t.getMessage, t)
        InternalServerError("Internal Server Error")
    }
  }

  private def getUUID: Try[UUID] = Try {
    UUID.fromString(params("uuid"))
  }.recoverWith { case t: Throwable =>
    Failure(new Exception(s"Bad Request. Invalid deposit id: ${ t.getClass.getName } ${ t.getMessage }"))
  }

  private def getPath: Try[Path] = Try {
    Paths.get(multiParams("splat").find(!_.trim.isEmpty).getOrElse(""))
  }.recoverWith { case t: Throwable =>
    Failure(new Exception(s"Bad Request. Invalid path: ${ t.getClass.getName } ${ t.getMessage }"))
  }

  private def getInputStream: Try[InputStream] = ???

  private def badInputStream(t: Throwable): ActionResult = {
    logger.error(s"badInputStream: ${ t.getMessage }", t)
    BadRequest(s"Bad Request. ${ t.getClass.getName } ${ t.getMessage }")
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
