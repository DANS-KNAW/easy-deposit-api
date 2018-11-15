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
package nl.knaw.dans.easy.deposit

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import org.scalatra.ActionResult

//TODO: candidate for dans-scala-lib

/**
 * Provides standard logging for servlet requests and responses masking sensitive data.
 * Usage is documented with unit tests in LoggerSpec.
 *
 * ==Rationale==
 *
 * The request are logged with before filters. An after filter would not see an [[org.scalatra.ActionResult]].
 * Its values are not saved in the implicit response provided by [[org.scalatra.ScalatraBase]]
 * as done by a trait for a [[org.scalatra.ScalatraServlet]] that extends [[org.scalatra.auth.ScentrySupport]].
 * See the last extensive readme version (documentation moved into an incomplete book and guides)
 * https://github.com/scalatra/scalatra/blob/6a614d17c38d19826467adcabf1dc746e3192dfc/README.markdown
 * sections #filters #action
 *
 * Mixing in [[org.scalatra.util.RequestLogging]] broke other unit test as it added a session header to some responses.
 */
package object logging {

  type HeaderMap = Map[String, Seq[String]]

  implicit private[logging] class MapExtensions[K, V](val stringMap: Map[K, V]) extends AnyVal {

    /** @return a toString like value with less class names */
    def makeString: String = {
      stringMap.map {
        case (k, v: Seq[_]) => k -> v.mkString("[", ", ", "]")
        case kv => kv
      }.mkString("[", ", ", "]")
    }
  }

  implicit class LogResponseSyntax(val actionResult: ActionResult) extends AnyVal {
    def logResponse(implicit request: HttpServletRequest,
                    response: HttpServletResponse,
                    responseLogger: AbstractResponseLogger): ActionResult = {
      responseLogger.logResponse(actionResult)
      actionResult
    }
  }
}
