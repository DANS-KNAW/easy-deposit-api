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
import org.scalatra.ScalatraBase
import org.scalatra.auth.ScentryStrategy

class BearerStrategy(protected override val app: ScalatraBase,
                     authenticationProvider: AuthenticationProvider
                    ) extends ScentryStrategy[User]
  with DebugEnhancedLogging {

  /** @return true if this strategy should be run. */
  override def isValid(implicit request: HttpServletRequest): Boolean = {
    // the boolean here and and option of authenticate don't allow for the error code granularity in rfc6750
    AuthHeader(request).scheme match {
      case Some("bearer") => true
      case _ => false
    }
  }

  override def authenticate()(implicit request: HttpServletRequest, response: HttpServletResponse): Option[User] = {
    val lastProxieIp = request.getRemoteAddr // TODO what is more available (other than headers which can be spoofed?)
    AuthHeader(request).payload.map(jwt =>
      ???
    ).getOrElse(None)
  }

  override def unauthenticated()(implicit request: HttpServletRequest, response: HttpServletResponse): Unit = ???
}
