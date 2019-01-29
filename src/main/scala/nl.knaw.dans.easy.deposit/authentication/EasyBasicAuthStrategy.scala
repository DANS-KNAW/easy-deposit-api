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
package nl.knaw.dans.easy.deposit.authentication

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState
import nl.knaw.dans.easy.deposit.logging._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.auth.strategy.BasicAuthStrategy
import org.scalatra.{ ScalatraBase, ServiceUnavailable, Unauthorized }

import scala.util.{ Failure, Success }

class EasyBasicAuthStrategy(protected override val app: ScalatraBase with AbstractResponseLogger,
                            authenticationProvider: AuthenticationProvider,
                            realm: String
                           ) extends BasicAuthStrategy[AuthUser](app, realm)
  with DebugEnhancedLogging {

  implicit val responseLogger: AbstractResponseLogger = app

  override def name: String = getClass.getSimpleName

  protected def validate(userName: String,
                         password: String)
                        (implicit request: HttpServletRequest,
                         response: HttpServletResponse
                        ): Option[AuthUser] = {
    def haltWithInvalidUser = {
      app halt Unauthorized(body = "invalid username/password").logResponse
    }

    def haltWithRegisteredUser = {
      app halt Unauthorized(body = "please confirm your registration first").logResponse
    }

    def haltWithFailure = {
      app halt ServiceUnavailable(body = "login service temporarily not available").logResponse
    }

    authenticationProvider.authenticate(userName, password) match {
      case Success(Some(AuthUser(id, UserState.active))) => Some(AuthUser(id, UserState.active))
      case Success(Some(AuthUser(_, UserState.registered))) => haltWithRegisteredUser
      case Success(Some(AuthUser(id, UserState.blocked))) =>
        logger.warn(s"blocked user '$id' tried to login")
        haltWithInvalidUser
      case Success(Some(AuthUser(id, state))) =>
        logger.error(s"unknown state '$state' for user '$id'")
        haltWithInvalidUser
      case Success(None) => haltWithInvalidUser
      case Failure(e) =>
        logger.error(e.getMessage, e)
        haltWithFailure
    }
  }

  protected def getUserId(user: AuthUser)
                         (implicit request: HttpServletRequest, response: HttpServletResponse): String = {
    user.id
  }
}
