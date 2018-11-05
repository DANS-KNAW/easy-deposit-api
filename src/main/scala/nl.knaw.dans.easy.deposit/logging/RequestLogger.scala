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

import javax.servlet.http.HttpServletRequest
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.{ Params, ScalatraBase }

// TODO candidate for dans-scala-lib

/**
 * Logs all received requests of a servlet.
 *
 * @example
 * {{{
 *     // The default is a safe mode in the sense of not leaking any possibly sensitive data into the logs.
 *     extends ScalatraServlet with RequestLogger
 * }}}
 * @example
 * {{{
 *     // override behaviour with additional traits and/or custom methods
 *     extends ScalatraServlet with RequestLogger with PlainRemoteAddress {
 *       override protected def formatRequestLog(implicit request: HttpServletRequest): String = {
 *         super.formatRequestLog(request) + " custom message"
 *       }
 *     }
 * }}}
 */
trait RequestLogger extends DebugEnhancedLogging {
  this: ScalatraBase =>

  /** Masks values of headers with a (case insensitive) name equal to "cookie" or ending with "authorization" */
  protected def maskHeaders(headers: Map[String, String]): String = headers.map {
    case (key, value) if key.toLowerCase == "cookie" => (key, maskCookieHeader(value))
    case (key, value) if key.toLowerCase.endsWith("authorization") => (key, maskAuthorizationHeader(value))
    case kv => kv
  }.mkString("[", ", ", "]")

  /** Keeps the key like "basic" and "digest" but masks the credentials. */
  protected def maskAuthorizationHeader(value: String): String = {
    value.replaceAll(" .+", " *****")
  }

  /** Masks the content of a cookie but not its name. */
  protected def maskCookieHeader(value: String): String = {
    val cookieName = value.replaceAll("=.*", "")
    val cookieValue = value.replaceAll(".*=", "")
    val maskedCookieValue = cookieValue.replaceAll("[^.]", "*") // replace everything but dots
    s"$cookieName=$maskedCookieValue"
  }

  /** Masks the values of the parameters with the name "login" and "password". */
  protected def maskLoginParameters(params: Params): String = params.map {
    case (key, _) if Seq("login", "password").contains(key.toLowerCase) => (key, "*****")
    case kv => kv
  }.mkString("[", ", ", "]")

  /**
   * https://www.bluecatnetworks.com/blog/ip-addresses-considered-personally-identifiable-information/
   * in case of link rot paste the url at the tail of https://web.archive.org/web/20181030102418/
   *
   * Services without public access might not need to mask.
   */
  protected def maskRemoteAddress(remoteAddress: String): String = remoteAddress
    .replaceAll("[0-9]+[.][0-9]+$", "**.**")

  protected def formatRequestLog(implicit request: HttpServletRequest): String = {
    val method = request.getMethod
    val requestURL = request.getRequestURL.toString
    val headers = maskHeaders(request.headers)
    val remoteAddr = maskRemoteAddress(request.getRemoteAddr)
    val maskedParams = maskLoginParameters(params)
    s"$method $requestURL remote=$remoteAddr; params=$maskedParams; headers=$headers"
  }

  before() {
    logger.info(formatRequestLog)
  }
}
