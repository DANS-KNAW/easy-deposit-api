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

trait CookieFormatter {

  /**
   * Formats the values of request headers with the (case insensitive) name "cookie"
   * and response headers with the (case insensitive) name "set-cookie".
   * The default implementation keeps the name of the first cookie, masks everything else
   * but dots and equal signs. Multiple equal signs may indicate there were multiple cookies.
   */
  protected def formatCookieValue(values: String): String = {
    val cookieName = values.replaceAll("=.*", "")
    val cookieValue = values.replaceAll(".*=", "")
    val maskedCookieValue = cookieValue.replaceAll("[^.=]", "*") // replace everything but dots
    s"$cookieName=$maskedCookieValue"
  }
}
