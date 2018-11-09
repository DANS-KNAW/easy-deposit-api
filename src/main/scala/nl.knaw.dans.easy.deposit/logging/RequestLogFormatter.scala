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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.ScalatraBase

import scala.collection.JavaConverters._

/**
 * @example
 * {{{
 *     // override behaviour with additional traits and/or custom methods
 *
 *     extends ScalatraServlet with RequestLogger with PlainRemoteAddress {
 *       override protected def formatRequestLog: String = {
 *         super.formatRequestLog(request) + " custom message"
 *       }
 *     }
 * }}}
 */
trait RequestLogFormatter extends DebugEnhancedLogging with CookieFormatter {
  this: ScalatraBase =>

  protected def headersToString(headers: HeaderMap): String = formatHeaders(headers).makeString

  /**
   * Formats request headers.
   * The default implementation has plain values for all headers
   * except those with a (case insensitive) name equal to "cookie" or ending with "authorization".
   */
  protected def formatHeaders(headers: HeaderMap): HeaderMap = {
    headers.map {
      // TODO use request.getCookies() instead?
      // TODO as ResponseLogFormatter.formatAuthHeaders
      case (name, values) if name.toLowerCase == "cookie" => name -> values.map(formatCookieValue)
      case (name, values) if name.toLowerCase.endsWith("authorization") => name -> values.map(formatValueOfAuthorizationHeader)
      case kv => kv
    }
  }

  /**
   * Formats the value of headers with a case insensitive name ending with "authorization".
   * The default implementation keeps the key like "basic", "digest" and "bearer" but masks the actual credentials.
   */
  protected def formatValueOfAuthorizationHeader(value: String): String = {
    value.replaceAll(" .+", " *****")
  }

  protected def parametersToString(params: Map[String, String]): String = formatParameters(params).makeString

  /**
   * Formats request parameters.
   * The default implementation has plain values for all parameters
   * except those with a (case insensitive) name equal to "login" or "password".
   */
  protected def formatParameters(params: Map[String, String]): Map[String, String] = {
    params.map {
      case (key, _) if Seq("login", "password").contains(key.toLowerCase) => (key, "*****")
      case kv => kv
    }
  }

  /**
   * Formats the value of the request property RemoteAddr.
   * The default implementation masks the last two components of the IP address.
   *
   * https://www.bluecatnetworks.com/blog/ip-addresses-considered-personally-identifiable-information/
   * in case of link rot paste the url at the tail of https://web.archive.org/web/20181030102418/
   *
   * Services without public access might not need to mask.
   */
  protected def formatRemoteAddress(remoteAddress: String): String = remoteAddress
    .replaceAll("[0-9]+[.][0-9]+$", "**.**")

  private def getHeaderMap: HeaderMap = {
    request.getHeaderNames.asScala.toSeq.map(
      name => name -> request.getHeaders(name).asScala.toSeq
    )
  }.toMap

  protected def formatRequestLog: String = {
    val method = request.getMethod
    val requestURL = request.getRequestURL.toString
    val formattedHeaders = headersToString(formatHeaders(getHeaderMap))
    val formattedParams = parametersToString(formatParameters(params))

    // TODO perhaps more of https://github.com/scalatra/scalatra/blob/2.7.x/core/src/main/scala/org/scalatra/util/RequestLogging.scala#L70-L85
    val formattedRemoteAddress = formatRemoteAddress(request.getRemoteAddr)

    s"$method $requestURL remote=$formattedRemoteAddress; params=$formattedParams; headers=$formattedHeaders"
  }
}
