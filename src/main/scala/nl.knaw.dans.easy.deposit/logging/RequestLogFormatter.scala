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
import org.scalatra.ScalatraBase

/**
 * @example
 * {{{
 *     // override behaviour with additional traits and/or custom methods
 *
 *     extends ScalatraServlet with RequestLogger with PlainRemoteAddress {
 *       override protected def formatRequestLog(implicit request: HttpServletRequest): String = {
 *         super.formatRequestLog(request) + " custom message"
 *       }
 *     }
 * }}}
 */
trait RequestLogFormatter extends DebugEnhancedLogging with CookieFormatter {
  this: ScalatraBase =>

  protected def maskedHeadersToString(headers: Map[String, String]): String = maskHeaders(headers).mkString("[", ", ", "]")

  /** Masks values of headers with a (case insensitive) name equal to "cookie" or ending with "authorization" */
  protected def maskHeaders(headers: Map[String, String]): Map[String, String] = {
    headers.map {
      case (key, value) if key.toLowerCase == "cookie" => (key, formatCookieValue(value))
      case (key, value) if key.toLowerCase.endsWith("authorization") => (key, maskAuthorizationHeader(value))
      case kv => kv
    }
  }

  /** Mask the value of headers with a case insensitive name ending with "authorization".
   * Keeps the key like "basic", "digest" and "Bearer" but masks the actual credentials.
   */
  protected def maskAuthorizationHeader(value: String): String = {
    value.replaceAll(" .+", " *****")
  }

  protected def maskedParametersToString(params: Map[String, String]): String = maskParameters(params).mkString("[", ", ", "]")

  /** Masks the values of the parameters with the name "login" and "password". */
  protected def maskParameters(params: Map[String, String]): Map[String, String] = {
    params.map {
      case (key, _) if Seq("login", "password").contains(key.toLowerCase) => (key, "*****")
      case kv => kv
    }
  }

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
    val headers = maskedHeadersToString(maskHeaders(request.headers))
    val remoteAddr = maskRemoteAddress(request.getRemoteAddr)
    val maskedParams = maskedParametersToString(maskParameters(params))
    s"$method $requestURL remote=$remoteAddr; params=$maskedParams; headers=$headers"
  }
}
