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

import java.util.Locale
import javax.servlet.http.HttpServletRequest

case class AuthHeader(request: HttpServletRequest) {
  // more or less from org.scalatra.BasicAuthStrategy

  private val parts = List("Authorization", "HTTP_AUTHORIZATION", "X-HTTP_AUTHORIZATION", "X_HTTP_AUTHORIZATION")
    .find(request.getHeader(_) != null) // TODO rfc6750: there should be only one
    .map { request.getHeader(_).split(" ", 2).toList } getOrElse Nil

  /** @return part before the first space of an authorisation header
   *          for possible values see:
   *          https://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml
   */
  def scheme: Option[String] = {
    parts
      .headOption
      .map(scheme => scheme.toLowerCase(Locale.ENGLISH))
  }

  def payload: Option[String] = parts.lastOption
}

