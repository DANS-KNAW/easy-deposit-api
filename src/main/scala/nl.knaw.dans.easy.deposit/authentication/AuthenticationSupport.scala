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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.ScalatraBase
import org.scalatra.auth.{ ScentryConfig, ScentrySupport }

trait AuthenticationSupport extends ScalatraBase
  with ScentrySupport[User]
  with DebugEnhancedLogging {
  self: ScalatraBase =>

  val realm = "easy-deposit" // TODO

  // TODO more than id? see also https://gist.github.com/casualjim/4400115#file-session_token_strategy-scala-L49-L50
  protected def fromSession: PartialFunction[String, User] = { case id: String => User(Map("uid" -> Seq(id))) }

  protected def toSession: PartialFunction[User, String] = { case usr: User => usr.id }

  def getAuthenticationProvider: AuthenticationProvider

  protected val scentryConfig: ScentryConfiguration = new ScentryConfig {
    override val login = "/sessions/new"
  }.asInstanceOf[ScentryConfiguration]

  protected def requireLogin(): Unit = {
    val addr = request.getRemoteAddr
    logger.info(s"remote=${ addr } local=${ request.getLocalAddr } $params ${ request.headers } ${ request.body }")
    if (!isAuthenticated) {
      redirect(scentryConfig.login)
    }
  }

  /**
   * If an unauthenticated user attempts to access a route which is protected by Scentry,
   * run the unauthenticated() method on the UserPasswordStrategy.
   */
  override protected def configureScentry: Unit = {
    scentry.unauthenticated {
      scentry.strategies("UserPassword").unauthenticated() // TODO which strategy?
    }
  }

  /**
   * Register auth strategies with Scentry. Any controller with this trait mixed in will attempt to
   * progressively use all registered strategies to log the user in, falling back if necessary.
   */
  override protected def registerAuthStrategies: Unit = {
    scentry.register("UserPassword", app => new UserPasswordStrategy(app, getAuthenticationProvider) {})
  }

}
