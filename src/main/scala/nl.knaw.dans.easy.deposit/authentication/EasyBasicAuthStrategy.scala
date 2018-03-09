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
import org.scalatra.auth.strategy.BasicAuthStrategy

object EasyBasicAuthStrategy {
  val name = "EasyBasicAuth"
}
class EasyBasicAuthStrategy(protected override val app: ScalatraBase, // in fact: AuthenticationSupport
                            authenticationProvider: AuthenticationProvider,
                            realm: String
                           ) extends BasicAuthStrategy[User](app, realm)
  with DebugEnhancedLogging {

  override def name: String = EasyBasicAuthStrategy.name

  /** @return true if this strategy should be run. */
  override def isValid(implicit request: HttpServletRequest): Boolean = {
    val valid = super.isValid
    logger.info(s"$name.isValid $valid")
    valid
  }
  override def authenticate()(implicit request: HttpServletRequest, response: HttpServletResponse): Option[User] = {
    logger.info(s"$name.authenticate")
    super.authenticate()
  }

  protected def validate(userName: String,
                         password: String)
                        (implicit request: HttpServletRequest,
                         response: HttpServletResponse
                        ): Option[User] = {
    logger.info(s"$name.validate: $userName, $password")// TODO don't leak, (doesn't get called even when isValid is true)
    authenticationProvider.getUser(userName, password)
  }

  protected def getUserId(user: User)
                         (implicit request: HttpServletRequest, response: HttpServletResponse): String = {
    logger.info(s"$name.getUserId: $user should be run: ${super.isValid}")
    user.id
  }
}
