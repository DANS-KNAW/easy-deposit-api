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

import nl.knaw.dans.easy.deposit.authentication.AuthenticationSupport._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.auth.ScentryAuthStore.CookieAuthStore
import org.scalatra.auth.{ Scentry, ScentryConfig, ScentrySupport }
import org.scalatra.{ CookieOptions, ScalatraBase }

import scala.collection.JavaConverters._

trait AuthenticationSupport extends ScalatraBase
  with ScentrySupport[User]
  with DebugEnhancedLogging {
  self: ScalatraBase =>


  // TODO more than id? see also https://gist.github.com/casualjim/4400115#file-session_token_strategy-scala-L49-L50
  protected def fromSession: PartialFunction[String, User] = { case id: String => User(Map("uid" -> Seq(id))) }

  protected def toSession: PartialFunction[User, String] = { case usr: User => usr.id }

  def getAuthenticationProvider: AuthenticationProvider

  def getCookieOptions: CookieOptions

  protected val scentryConfig: ScentryConfiguration = new ScentryConfig {
    override val login = "/auth/signin"
  }.asInstanceOf[ScentryConfiguration]

  protected def requireLogin(): Unit = {
    logger.info(s"${ request.getMethod } ${ request.getRequestURL } remote=${ request.getRemoteAddr } params=$params headers=${ request.headers } body=${ request.body }")
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
      halt(BAD_REQUEST_400, "Please provide at most one authentication method")
    }
  }

  /**
   * If an unauthenticated user attempts to access a route which is protected by Scentry,
   * run the unauthenticated() method on the UserPasswordStrategy.
   */
  override protected def configureScentry {

    scentry.store = new CookieAuthStore(self)(getCookieOptions) {
      override def set(value: String)(implicit request: HttpServletRequest, response: HttpServletResponse) {
        cookies.update(Scentry.scentryAuthKey, value)(getCookieOptions)
      }

      override def get(implicit request: HttpServletRequest, response: HttpServletResponse): String = {
        cookies.get(Scentry.scentryAuthKey).getOrElse("")
      }

      override def invalidate()(implicit request: HttpServletRequest, response: HttpServletResponse) {
        // See also https://github.com/scalatra/scalatra-website-examples/blob/d1728bace838162e8331f9f001767a2d505f6a2a/2.6/http/scentry-auth-demo/src/main/scala/com/constructiveproof/example/auth/strategies/RememberMeStrategy.scala#L76
        cookies.delete(Scentry.scentryAuthKey)(CookieOptions(path = "/"))
      }
    }
    scentry.unauthenticated { scentry.strategies("UserPassword" /*TODO ???*/).unauthenticated() }
  }

  /**
   * Register auth strategies with Scentry. Any controller with this trait mixed in will attempt to
   * progressively use all registered strategies to log the user in, falling back if necessary.
   */
  override protected def registerAuthStrategies: Unit = {
    scentry.register("BasicAuthentication", app => new EasyBasciAuthStrategy(app, getAuthenticationProvider, realm) {})
    scentry.register("UserPassword", app => new UserPasswordStrategy(app, getAuthenticationProvider) {})
    scentry.register("SessionToken", app => new SessionTokenStrategy(app) {})
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
