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

import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.easy.deposit.authentication.{ AuthenticationProvider, AuthenticationSupport }
import org.eclipse.jetty.http.HttpStatus
import org.scalatra.CookieOptions

abstract class AbstractProtectedServlet(app: EasyDepositApiApp) extends AuthenticationSupport {

  override def getAuthenticationProvider: AuthenticationProvider = app.authentication

  override def getCookieOptions: CookieOptions = app.authCookieOptions

  override def getTokenConfig: TokenConfig = app.tokenConfig

  before() {
    trace("abstract")
    if (!isAuthenticated) {
      halt(HttpStatus.FORBIDDEN_403, "missing, invalid or expired credentials")
    }
  }
}
