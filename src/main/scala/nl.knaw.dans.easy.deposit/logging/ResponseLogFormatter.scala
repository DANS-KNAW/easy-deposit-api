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
package nl.knaw.dans.easy.deposit.logging

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.ActionResult

/**
 * The first example block enables the subsequent examples.
 *
 * Put the implicit val in a [[org.scalatra.ScalatraServlet]] and/or a in a trait with "self: [[org.scalatra.ScalatraBase]]"
 * such as a trait extending [[org.scalatra.auth.ScentrySupport]].
 * Customize the formatter instance by adding traits like PlainAuthHeaders or overriding methods.
 * Make the instance in a trait private if the actual servlets need an instance with other customization.
 *
 * @example
 * {{{
 *   implicit val responseLogFormatter: ResponseLogFormatter = new ResponseLogFormatter {}
 *
 *   package object servlets extends DebugEnhancedLogger {
 *     implicit class RichActionResult(val actionResult: ActionResult) extends AnyVal {
 *       def logResponse(implicit request: HttpServletRequest,
 *                       response: HttpServletResponse,
 *                       formatter: ResponseLogFormatter
 *                      ): ActionResult = {
 *         logger.info(responseLogFormatter.logActionResult(actionResult))
 *         actionResult
 *       }
 *     }
 *   }
 * }}}
 *
 * An after method would not see an [[ActionResult]].
 * Its values are not saved in the implicit response provided by [[org.scalatra.ScalatraBase]]
 * as done by a trait for a [[org.scalatra.ScalatraServlet]] that extends [[org.scalatra.auth.ScentrySupport]].
 * See the last extensive readme version (documentation moved into an incomplete book and guides)
 * https://github.com/scalatra/scalatra/blob/6a614d17c38d19826467adcabf1dc746e3192dfc/README.markdown
 * sections #filters #action
 * @example
 * {{{
 *   // usage in servlets
 *
 *   ... extends ScalatraServlet {
 *     get(???) {
 *       ???
 *       Ok(body = "hello world").logResponse
 *     }
 *   }
 * }}}
 *
 * An after method would not be executed at all after a halt.
 * @example
 * {{{
 *   // usage in a trait for servlets that extends ScentrySupport.
 *
 *   halt(Unauthorized(???).logResponse)
 * }}}
 */
trait ResponseLogFormatter extends CookieMasker {

  /** Assembles the content for a log line.
   *
   * @param actionResult the response created by the servlet
   * @param request      some info is used as a kind of bracketing the response log line with the request log line
   * @param response     here we might find some info created by a [[org.scalatra.auth.ScentrySupport]] trait
   *                     for a [[org.scalatra.ScalatraServlet]].
   * @return
   */
  def formatResponseLog(actionResult: ActionResult)
                       (implicit request: HttpServletRequest, response: HttpServletResponse): String = {
    s"${ request.getMethod } returned status=${ actionResult.status }; authHeaders=${ authHeadersToString(response) }; actionHeaders=${ actionHeadersToString(actionResult) }"
  }

  protected def actionHeadersToString(actionResult: ActionResult): String =
    maskActionHeaders(actionResult).mkString("[", ",", "]")

  protected def maskActionHeaders(actionResult: ActionResult): Map[String, String] =
    actionResult.headers

  protected def authHeadersToString(response: HttpServletResponse): String =
    maskAuthHeaders(response).mkString("[", ", ", "]")

  /** masks the values for REMOTE_USER and Set-Cookie */
  protected def maskAuthHeaders(response: HttpServletResponse): Map[String, String] = {
    response.getHeaderNames.toArray().map {
      case name: String if "set-cookie" == name.toLowerCase => (name, maskCookieHeader(response.getHeader(name)))
      case name: String if "remote_user" == name.toLowerCase => (name, maskRemoteUserHeader(response.getHeader(name)))
      case name: String => name -> response.getHeader(name)
    }.toMap
  }

  protected def maskRemoteUserHeader(value: String): String = "*****"
}
