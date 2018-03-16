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
import org.joda.time.DateTime
import org.scalatra._

class AuthenticationServlet(app: EasyDepositApiApp) extends AuthenticationSupport {

  override def getAuthenticationProvider: AuthenticationProvider = app.authentication

  override def getCookieOptions: CookieOptions = app.authCookieOptions

  override def getTokenConfig: TokenConfig = app.tokenConfig

  get("/signin") {
    // to manually test the UserPasswordStrategy with a browser but no user interface
    if (isAuthenticated)
      Ok(s"$user is signed in, ${ new DateTime() }")
    else {
      contentType = "text/html"
      <html>
        <body>
          <form action="/auth/signin" method="post">
            <p><label for="login">login id</label><input type="text" name="login" id="login" autofocus="true" /></p>
            <p><label for="password">password</label><input type="password" name="password" id="password"/></p>
            <p><input type="submit"/></p>
          </form>
        </body>
      </html>
    }
  }

  // TODO state is changed, but get supports an easy demo...
  get("/signout") { signout }
  put("/signout") { signout }
  post("/signout") { signout }

  private def signout = {
    logOut() // destroys the scentry cookie
    Ok("you are signed out")
  }

  post("/signin") {
    authenticate()
    if (isAuthenticated) {
      Ok(s"signed in")
    }
    else {
      Forbidden("invalid credentials")
    }
  }
}
