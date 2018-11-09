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
import org.scalatra.ActionResult
import org.scalatra.util.RequestLogging

import scala.collection.JavaConverters._

trait ResponseLogFormatter extends CookieFormatter {

  /** Assembles the content for a log line.
   *
   * Adding the logResponse method to [[ActionResult]] for the following examples is explained at package level.
   *
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
   * @param actionResult the response created by the servlet
   * @param request      some info is used as a kind of bracketing the response log line with the request log line
   * @param response     here we might find some info created by a [[org.scalatra.auth.ScentrySupport]] trait
   *                     for a [[org.scalatra.ScalatraServlet]].
   * @return
   */
  def formatResponseLog(actionResult: ActionResult)
                       (implicit request: HttpServletRequest, response: HttpServletResponse): String = {
    s"${ request.getMethod } returned status=${ actionResult.status }; authHeaders=${ authHeadersToString }; actionHeaders=${ actionHeadersToString(actionResult) }"
  }

  protected def actionHeadersToString(actionResult: ActionResult): String =
    formatActionHeaders(actionResult).makeString

  protected def formatActionHeaders(actionResult: ActionResult): Map[String, String] =
    actionResult.headers

  protected def authHeadersToString(implicit response: HttpServletResponse): String =
    formatAuthHeaders(response).makeString

  /** Formats the values of headers with (case insensitive) name REMOTE_USER and Set-Cookie */
  protected def formatAuthHeaders(implicit response: HttpServletResponse): HeaderMap = {
    response.getHeaderNames.toArray().map {
      case name: String if "set-cookie" == name.toLowerCase => (name, getHeaderSeq(name).map(formatCookieValue))
      case name: String if "remote_user" == name.toLowerCase => (name, getHeaderSeq(name).map(formatRemoteUserValue))
      case name: String => name -> getHeaderSeq(name)
    }.toMap
  }

  private def getHeaderSeq(name: String)(implicit response: HttpServletResponse) = {
    Option(response.getHeaders(name)).map(_.asScala.toSeq).getOrElse(Seq.empty)
  }

  protected def formatRemoteUserValue(value: String): String = "*****"
}
