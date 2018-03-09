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
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.auth.ScentryAuthStore.CookieAuthStore
import org.scalatra.auth.{ ScentryConfig, ScentrySupport }
import org.scalatra.{ CookieOptions, ScalatraBase }

import scala.collection.JavaConverters._

trait AuthenticationSupport extends ScalatraBase
  with ScentrySupport[User]
  with ServletEnhancedLogging {
  self: ScalatraBase =>

  // TODO see also https://gist.github.com/casualjim/4400115#file-session_token_strategy-scala-L49-L50
  override protected def fromSession: PartialFunction[String, User] = { case id: String => User.fromToken(id) }

  override protected def toSession: PartialFunction[User, String] = { case usr: User => usr.id }

  def getAuthenticationProvider: AuthenticationProvider

  def getCookieOptions: CookieOptions

  override protected val scentryConfig: ScentryConfiguration = new ScentryConfig {
    override val login = "/auth/signin"
  }.asInstanceOf[ScentryConfiguration]

  protected def requireLogin(): Unit = {
    noMultipleAuthentications()
    if (!scentry.isAuthenticated) {
      redirect(scentryConfig.login) // TODO don't loose form data when session timed out
    }
  }

  private def noMultipleAuthentications(): Unit = {
    val authenticationHeaders = request.getHeaderNames.asScala.toList
      .map(_.toLowerCase)
      .filter(h => headers.contains(h))
    //noinspection ComparingLength
    if (authenticationHeaders.size > 1 || scentry.strategies.values.count(_.isValid) > 1) {
      logger.info(s"found authentication headers [$authenticationHeaders] and methods [${ scentry.strategies.values.withFilter(_.isValid).map(_.name) }]")
      halt(BAD_REQUEST_400, "Invalid authentication")
    }
  }

  /**
   * If an unauthenticated user attempts to access a route which is protected by Scentry,
   * run the unauthenticated() method on the UserPasswordStrategy.
   */
  override protected def configureScentry {

    scentry.store = new CookieAuthStore(self)(getCookieOptions)

    // TODO only overridden by EasyBasicAuthStrategy.super Default if none af the strategies applied? What about the redirect in requireLogin?
    scentry.unauthenticated { scentry.strategies("UserPassword").unauthenticated() }
  }

  /**
   * Register auth strategies with Scentry. Any controller with this trait mixed in will attempt to
   * progressively use all registered strategies to log the user in, falling back if necessary.
   */
  override protected def registerAuthStrategies: Unit = {
    scentry.register(UserPasswordStrategy.name, _ => new UserPasswordStrategy(self, getAuthenticationProvider))
    scentry.register(SessionTokenStrategy.name, _ => new SessionTokenStrategy(self))

    // after user/password otherwise getUserId gets called
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
