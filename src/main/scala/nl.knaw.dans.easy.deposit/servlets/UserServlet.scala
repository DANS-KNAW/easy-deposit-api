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

import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.easy.deposit.docs.JsonUtil._
import nl.knaw.dans.easy.deposit.docs.UserInfo
import nl.knaw.dans.easy.deposit.logging._
import nl.knaw.dans.lib.error._
import org.scalatra._

import scala.util.Try

class UserServlet(app: EasyDepositApiApp) extends ProtectedServlet(app) {

  get("/") {
    Try(app.getUser(user.id)).flatten // no throw should slip through
      .map(properties => Ok(toJson(UserInfo(properties))))
      .getOrRecover(respond).logResponse
  }
  put("/") {
    Try(for {
      user <- UserInfo(request.body)
      _ <- Try(???)
    } yield Ok(???)).flatten
      .getOrRecover(respond).logResponse
  }

  private def respond(t: Throwable): ActionResult = t match {
    case e: InvalidDocumentException => badDocResponse(e)
    case _ => notExpectedExceptionResponse(t)
  }
}


