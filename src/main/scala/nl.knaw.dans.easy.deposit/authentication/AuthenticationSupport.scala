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

import nl.knaw.dans.easy.deposit.authentication.AuthenticationSupport._
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.auth.ScentryAuthStore.CookieAuthStore
import org.scalatra.auth.{ Scentry, ScentryConfig, ScentrySupport }
import org.scalatra.{ CookieOptions, ScalatraBase, ScalatraServlet }

import scala.collection.JavaConverters._

trait AuthenticationSupport extends ScalatraServlet
  with ScalatraBase
  with ScentrySupport[AuthUser]
  with ServletEnhancedLogging
  with TokenSupport {
  self: ScalatraBase =>

  /** read method name as: fromCookie, see scentryStore in configureScentry */
  override protected def fromSession: PartialFunction[String, AuthUser] = {
    case token: String =>
      trace(token)
      toUser(token)
        .doIfFailure { case t => logger.info(s"invalid authentication: ${ t.getClass } ${ t.getMessage }") }
        .getOrElse(null)
  }

  /** read method name as: toCookie, see scentryStore in configureScentry */
  override protected def toSession: PartialFunction[AuthUser, String] = {
    case user: AuthUser => encode(user)
  }

  def getAuthenticationProvider: AuthenticationProvider

  def getCookieOptions: CookieOptions

  override protected val scentryConfig: ScentryConfiguration = new ScentryConfig {
    override val login = "/auth/signin"
  }.asInstanceOf[ScentryConfiguration]

  def isAuthenticated: Boolean = {
    noMultipleAuthentications() // may come after calling ldap
    scentry.isAuthenticated
  }

  def authenticate(): Option[AuthUser] = {
    noMultipleAuthentications() // comes before calling ldap
    scentry.authenticate()
  }

  private def noMultipleAuthentications(): Unit = {

    // size >=1 means EasyBasicAuthStrategy is valid
    val authenticationHeaders = request
      .getHeaderNames.asScala.toList
      .map(_.toLowerCase)
      .filter(h => headers.contains(h))

    val hasAuthCookie = request.getCookies.exists(_.getName == Scentry.scentryAuthKey)
    trace(hasAuthCookie, authenticationHeaders)
    val validStrategies = scentry.strategies.values.filter(_.isValid)
    if ((hasAuthCookie, validStrategies.size, authenticationHeaders.size) match {
      case (false, 0, 0) => false
      case (true, 0, 0) => false
      case (true, 1, _) => true
      case (true, _, 1) => true
      case (_, nrOfStrategies, nrOfHeaders) if nrOfStrategies > 1 || nrOfHeaders > 1 => true
      case _ => false
    }) {
      logger.info(s"hasAuthCookie=$hasAuthCookie and/or authentication headers [$authenticationHeaders] and/or methods [${ validStrategies.map(_.name) }]")
      halt(BAD_REQUEST_400, "Invalid authentication")
    }
    trace(hasAuthCookie, authenticationHeaders, validStrategies.map(_.name))
  }

  /**
   * If an unauthenticated user attempts to access a route which is protected by Scentry,
   * run the unauthenticated() method on the UserPasswordStrategy.
   */
  override protected def configureScentry {

    scentry.store = new CookieAuthStore(self)(getCookieOptions)

    // TODO overridden by EasyBasicAuthStrategy.super Default if none of the strategies applied?
    scentry.unauthenticated { scentry.strategies("UserPassword").unauthenticated() }
  }

  /**
   * Register auth strategies with Scentry. Any controller with this trait mixed in will attempt to
   * progressively use all registered strategies to log the user in, falling back if necessary.
   */
  override protected def registerAuthStrategies: Unit = {
    // don't need a token strategy: AuthenticationSupport$$anonfun$fromSession can return null
    scentry.register(UserPasswordStrategy.name, _ => new UserPasswordStrategy(self, getAuthenticationProvider))

    // after user/password otherwise getUserId gets called even if isValid is false
    scentry.register(EasyBasicAuthStrategy.name, _ => new EasyBasicAuthStrategy(self, getAuthenticationProvider, realm))
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
