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

import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra._

class AuthenticationServlet(app: EasyDepositApiApp) extends ScalatraServlet
  with AuthenticationSupport
  with DebugEnhancedLogging {

  override def getAuthenticationProvider: AuthenticationProvider = app.authentication

  override def getCookieOptions: CookieOptions = app.cookieOptions

  before() {
    logger.info(s"${ request.getMethod } ${ request.getPathInfo } remote=${ request.getRemoteAddr } params=$params headers=${ request.headers } body=${ request.body }")
  }

  after() {
    logger.info(s"response.staus=${ response.getStatus } headers=${ response.headers }")
  }

  get("/signin") {
    if (isAuthenticated) redirect("/deposit")

    contentType = "text/html"
    <html>
      <body>
        <form action="/auth" method="post">
          <p><label for="login">login id</label><input type="text" name="login" id="login"/></p>
          <p><label for="password">password</label><input type="password" name="password" id="password"/></p>
          <p><input type="submit"/></p>
        </form>
      </body>
    </html>
  }

  get("/signout") { signout }
  put("/signout") { signout }
  post("/signout") { signout }

  private def signout = {
    scentry.store.invalidate() // TODO state is changed, but get supports an easy demo...
    redirect("/")
  }

  post("/") {
    scentry.authenticate()

    if (isAuthenticated) {
      redirect("/deposit")
    }
    else {
      redirect("/auth/signin")
    }
  }
}
