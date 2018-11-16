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
import org.scalatra.{ ActionResult, ScalatraBase }

import scala.collection.JavaConverters._

trait ResponseLogFormatter extends CookieFormatter {
  this: ScalatraBase =>

  protected def authHeadersToString(headerMap: HeaderMap): String =
    formatAuthHeaders(headerMap).makeString

  /**
   * Formats response headers as far as prepared by traits of the servlet.
   * The default implementation has plain values for all headers
   * except those with a (case insensitive) name equal to "REMOTE_USER" or "Set-Cookie" */
  protected def formatAuthHeaders(headerMap: HeaderMap): HeaderMap = {
    headerMap.map {
      case (name, values) if "set-cookie" == name.toLowerCase => name -> values.map(formatCookieValue)
      case (name, values) if "remote_user" == name.toLowerCase => name -> values.map(formatRemoteUserValue)
      case nameValues => nameValues
    }
  }

  private def getHeaderMap: HeaderMap = {
    // looks the same method as for RequestLogFormatter, but everywhere different classes
    response.getHeaderNames.asScala.toSeq.map(
      name => name -> Option(response.getHeaders(name)).map(_.asScala.toSeq).getOrElse(Seq.empty)
    ).toMap
  }

  protected def actionHeadersToString(actionResult: ActionResult): String =
    formatActionHeaders(actionResult).makeString

  protected def formatActionHeaders(actionResult: ActionResult): Map[String, String] =
    actionResult.headers

  protected def formatRemoteUserValue(value: String): String = "*****"

  /**
   * Assembles the content for a log line.
   *
   * Adding the logResponse method to [[ActionResult]] for the following examples
   * is explained by [[nl.knaw.dans.easy.deposit.logging]].
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
   * @return a formatted log line
   */
  def formatResponseLog(actionResult: ActionResult)
                       (implicit request: HttpServletRequest, response: HttpServletResponse): String = {
    val method = request.getMethod
    val status = actionResult.status
    val formattedAuthHeaders = authHeadersToString(getHeaderMap)
    val formattedActionHeaders = actionHeadersToString(actionResult)
    s"${ method } returned status=${ status }; authHeaders=$formattedAuthHeaders; actionHeaders=$formattedActionHeaders"
  }
}
