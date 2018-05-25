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

import java.net.URL

import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState
import nl.knaw.dans.easy.deposit.authentication.AuthenticationSupport._
import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import nl.knaw.dans.lib.error._
import org.scalatra._
import org.scalatra.auth.ScentryAuthStore.CookieAuthStore
import org.scalatra.auth.{ ScentryConfig, ScentrySupport }

trait AuthenticationSupport extends ScentrySupport[AuthUser] {
  self: ScalatraBase with TokenSupport with AuthConfig =>

  /** read method name as: fromCookie, see configured scentry.store */
  override protected def fromSession: PartialFunction[String, AuthUser] = {
    case token: String => decodeJWT(token)
      .doIfSuccess { user => scentry.store.set(encodeJWT(user)) } // refresh cookie
      .doIfFailure { case t => logger.info(s"invalid authentication: ${ t.getClass } ${ t.getMessage }") }
      .getOrElse(null)
  }

  /** read method name as: toCookie, see configured scentry.store */
  override protected def toSession: PartialFunction[AuthUser, String] = {
    case user: AuthUser =>
      user.state match {
        case UserState.registered => halt(Unauthorized("Please confirm your email.").logResponse)
        case UserState.blocked => halt(Unauthorized("invalid credentials").logResponse)
        case UserState.active => encodeJWT(user)
      }
  }

  override protected val scentryConfig: ScentryConfiguration =
    new ScentryConfig {}.asInstanceOf[ScentryConfiguration]

  /** Successful authentications will result in a cookie. */
  override protected def configureScentry {

    // headers can be spoofed and should not be trusted
    // it looks like the request URL is not constructed from headers
    val returnCookieOverHttpsOnly = new URL(request.getRequestURL.toString).getProtocol == "https"

    // avoid name clash with implicit def cookieOptions
    val cookieConfig = CookieOptions(
      domain = "", // limits which server get the cookie // TODO by default the host who sent it?
      path = "/", // limits which route gets the cookie, TODO configure and/or from mounts in Service class
      maxAge = getProperties.getInt("auth.cookie.expiresIn", 10), // seconds, MUST be same default as in TokenSupport
      secure = returnCookieOverHttpsOnly,
      httpOnly = true // JavaScript can't get the cookie
      // version = 0 // obsolete? https://stackoverflow.com/questions/29124177/recommended-set-cookie-version-used-by-web-servers-0-1-or-2#29143128
    )
    logger.info(s"authCookieOptions: $cookieConfig")
    scentry.store = new CookieAuthStore(self)(cookieConfig)
  }

  /**
   * Register auth strategies with Scentry. Any controller with this trait mixed in will attempt to
   * progressively use all registered strategies to log the user in, falling back if necessary.
   */
  override protected def registerAuthStrategies: Unit = {
    scentry.register(UserPasswordStrategy.getClass.getSimpleName, _ => new UserPasswordStrategy(self, getAuthenticationProvider))
    scentry.register(EasyBasicAuthStrategy.getClass.getSimpleName, _ => new EasyBasicAuthStrategy(self, getAuthenticationProvider, realm))

    // don't need a cookie-with-token strategy:
    // scentry uses the configured scentry.store and the implementation of from/to-Session methods
  }

  before() {
    authenticate()
  }

  /** Halts request processing in case of trouble. */
  def login() {
    if (!isAuthenticated) {
      halt(Unauthorized("invalid credentials").logResponse)
    }
  }
}
object AuthenticationSupport {

  // the same username and password combination should work for any page within the same realm
  private val realm = "easy-deposit"

  private val headers = List( // among others values from org.scalatra.BasicAuthStrategy
    "Authorization",
    "HTTP_AUTHORIZATION",
    "X-HTTP_AUTHORIZATION",
    "X_HTTP_AUTHORIZATION"
  ).map(_.toLowerCase)
}
