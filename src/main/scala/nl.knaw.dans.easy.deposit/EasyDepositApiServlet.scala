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

import nl.knaw.dans.easy.deposit.authentication.{ AuthenticationProvider, AuthenticationSupport }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra._

class EasyDepositApiServlet(app: EasyDepositApiApp) extends ScalatraServlet
  with AuthenticationSupport
  with DebugEnhancedLogging {

  override def getAuthenticationProvider: AuthenticationProvider = app.authentication
  override def getCookieOptions: CookieOptions = app.cookieOptions

  before() {
    requireLogin()// also logs the request
  }

  after () {
    logger.info(s"response.staus=${ response.getStatus } headers=${ response.headers }")
  }

  get("/") {
    contentType = "text/plain"
    Ok(s"$user : EASY Deposit Api Service running...")
  }
}
