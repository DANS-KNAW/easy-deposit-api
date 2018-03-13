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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.error._
import org.apache.commons.lang.StringUtils.isNotBlank
import org.scalatra.auth.{ Scentry, ScentryStrategy }

import scala.util.{ Failure, Success }

object SessionTokenStrategy {
  val name = "SessionToken"
}
class SessionTokenStrategy(protected override val app: AuthenticationSupport,
                          )(implicit request: HttpServletRequest,
                            response: HttpServletResponse)
  extends ScentryStrategy[AuthUser]
    with DebugEnhancedLogging {

  override def name: String = SessionTokenStrategy.name

  private def getToken(implicit request: HttpServletRequest): Option[String] = {
    request.getCookies
      .find(_.getName == Scentry.scentryAuthKey) // TODO add issued to token and filter on maxAge?
      .map(_.getValue)
      .find(isNotBlank)
  }

  /** @return true if this strategy should be run. */
  override def isValid(implicit request: HttpServletRequest): Boolean = {
    getToken.isDefined
  }

  override def authenticate()
                           (implicit request: HttpServletRequest,
                            response: HttpServletResponse
                           ): Option[AuthUser] = {
    // TODO doesn't get called, causing stacktrace at AuthenticationSupport$$anonfun$fromSession
    getToken.flatMap(toMaybeAuthUser)
  }

  private def toMaybeAuthUser(token: String) = {
    app.toUser(token)
      .doIfFailure{case e => logger.info(s"invalid token [$token]: $e")}
      .map(Option(_))
      .getOrElse(None)
  }
}

