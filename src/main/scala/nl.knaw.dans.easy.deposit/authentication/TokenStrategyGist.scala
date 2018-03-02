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

import java.util.UUID
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

import org.apache.commons.lang.StringUtils.isNotBlank
import org.scalatra.auth.ScentryAuthStore.CookieAuthStore
import org.scalatra.auth.{ Scentry, ScentryStrategy, ScentrySupport }
import org.scalatra.{ CookieOptions, ScalatraBase, ScalatraContext }

// trying to make head or tails from https://gist.github.com/casualjim/4400115 (2012)

case class MyUser(id: Int, login: String, password: String, token: String = UUID.randomUUID.toString)

object UsersDao {

  def fromToken(token: String): Option[MyUser] = ???

}

object SessionTokenStrategy {
  val HeaderKey = "X-API-KEY"
  val ParamsKey = "api_key"
  val CookieKey = "scalatra.auth"
}
class SessionTokenStrategy(protected val app: ScalatraBase) extends ScentryStrategy[MyUser] {

  import SessionTokenStrategy._

  private def getToken(implicit request: HttpServletRequest) = {
    (Option(app.request.getHeader(HeaderKey)) orElse
      app.params.get(ParamsKey) orElse
      app.cookies.get(CookieKey)
      ).find(isNotBlank)
  }

  override def isValid(implicit request: HttpServletRequest): Boolean = getToken.isDefined

  override def authenticate()(implicit request: HttpServletRequest,
                              response: HttpServletResponse): Option[MyUser] = {
    getToken.flatMap(UsersDao.fromToken)
  }
}

class PasswordStrategy(protected val app: ScalatraBase,
                       authenticationProvider: AuthenticationProvider) extends ScentryStrategy[MyUser] {
  override def isValid(implicit request: HttpServletRequest): Boolean = {
    app.params.get("login").isDefined && app.params.get("password").isDefined
  }

  override def authenticate()(implicit request: HttpServletRequest, response: HttpServletResponse): Option[MyUser] = authenticationProvider
    .getUser(app.params("login"), app.params("password"))
    .map { u => MyUser(1, u.id, "password") }
}

trait MyAuthentication extends ScalatraBase with ScentrySupport[MyUser] {

  val self: ScalatraContext = this

  def getAuthenticationProvider: AuthenticationProvider

  protected def fromSession: PartialFunction[String, MyUser] = { case token: String => UsersDao.fromToken(token).get } // TODO unsafe
  protected def toSession: PartialFunction[MyUser, String] = { case usr: MyUser => usr.token }

  /**
   * Registers authentication strategies.
   */
  override protected def configureScentry {
    val authCookieOptions = CookieOptions(httpOnly = true)

    scentry.store = new CookieAuthStore(self)(authCookieOptions) {
      override def set(value: String)(implicit request: HttpServletRequest, response: HttpServletResponse) {
        super.set(value)
        response.setHeader("X-SCALATRA-AUTH", value)
      }

      override def get(implicit request: HttpServletRequest, response: HttpServletResponse): String = {
        val cookie = super.get
        if (cookie == null || cookie.trim.isEmpty) request.getHeader("X-SCALATRA-AUTH")
        else cookie
      }

      override def invalidate()(implicit request: HttpServletRequest, response: HttpServletResponse) {
        self.cookies.update(Scentry.scentryAuthKey, "")(authCookieOptions.copy(maxAge = 0))
        response.setHeader("X-SCALATRA-AUTH", null)
      }
    }
    scentry.unauthenticated { scentry.strategies("user_password" /*???*/).unauthenticated() }
  }

  override protected def registerAuthStrategies: Unit = {
    scentry.register("user_password", app => new PasswordStrategy(app, getAuthenticationProvider))
    scentry.register("session_token", app => new SessionTokenStrategy(app))
  }
}