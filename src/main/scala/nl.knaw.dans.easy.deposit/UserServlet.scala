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

import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.easy.deposit.components.Json._
import nl.knaw.dans.easy.deposit.components.User
import org.scalatra._

import scala.util.Try

class UserServlet(app: EasyDepositApiApp) extends AbstractAuthServlet(app) {

  before() {
    if (!isAuthenticated) {
      halt(Forbidden("missing, invalid or expired credentials").logResponse)
    }
  }

  get("/") {
    Try(app.getUser(user.id)).flatten // no throw should slip through
      .map(map => Ok(toJson(User(map))))
      .getOrRecover(respond)
  }
  put("") {
    (for {
      user <- getUser(request.body)
      _ <- Try(???).flatten
    } yield Ok(???))
      .getOrRecover(respond)
  }

  private def respond(t: Throwable): ActionResult = t match {
    case _: InvalidDocument => badDocResponse(t)
    case _ => internalErrorResponse(t)
  }
}

