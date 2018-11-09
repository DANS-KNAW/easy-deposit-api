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

//TODO: candidate for dans-scala-lib

/**
 * Provides standard logging for servlet requests and responses masking sensitive data.
 *
 * ==Requests==
 *
 * All requests are logged at info level by adding a trait to servlets:
 * {{{
 *   class MyServlet extends ScalatraServlet with RequestLogger {???}
 * }}}
 * To change the log level (or use another logger) define your own RequestLogger like:
 * {{{
 *   trait DebugRequestLogger extends RequestLogFormatter {
 *     this: ScalatraBase =>before() { logger.debug(formatRequestLog) }
 *   }
 * }}}
 * The [[nl.knaw.dans.easy.deposit.logging.RequestLogFormatter]] and unit tests
 * documents hooks to customize the content of the logged message.
 *
 * ==Responses==
 *
 * Each response must be logged explicitly by a method added to the class [[org.scalatra.ActionResult]].
 * For flexible use of the customization hooks move the implicit instance of the
 * [[nl.knaw.dans.easy.deposit.logging.ResponseLogFormatter]]
 * to the servlets and/or traits with "self: [[org.scalatra.ScalatraBase]]".
 * Making these instances private in traits allows the servlets to define their own formatting.
 * {{{
 *    package object servlets extends DebugEnhancedLogger {
 *      implicit val formatter: ResponseLogFormatter = new ResponseLogFormatter {}
 *      implicit class RichActionResult(val actionResult: ActionResult) extends AnyVal {
 *        def logResponse(implicit request: HttpServletRequest,
 *                        response: HttpServletResponse,
 *                        formatter: ResponseLogFormatter
 *                       ): ActionResult = {
 *          logger.info(formatter.logActionResult(actionResult))
 *          actionResult
 *        }
 *      }
 *    }
 * }}}
 *
 * ==Before and/or after==
 *
 * The request are logged with before filters. An after filter would not see an [[org.scalatra.ActionResult]].
 * Its values are not saved in the implicit response provided by [[org.scalatra.ScalatraBase]]
 * as done by a trait for a [[org.scalatra.ScalatraServlet]] that extends [[org.scalatra.auth.ScentrySupport]].
 * See the last extensive readme version (documentation moved into an incomplete book and guides)
 * https://github.com/scalatra/scalatra/blob/6a614d17c38d19826467adcabf1dc746e3192dfc/README.markdown
 * sections #filters #action
 *
 * Mixing in [[org.scalatra.util.RequestLogging]] broke unit test as it added a session header to some responses.
 */
package object logging {

  type HeaderMap = Map[String, Seq[String]]

  implicit class StringMapExtensions(val stringMap: Map[String, String]) extends AnyVal {
    def makeString: String = stringMap.mkString("[", ", ", "]")
  }

  /** Extension for [[HeaderMap]] and [[org.scalatra.MultiParams]]. */
  implicit class NestedStringMapExtensions(val headerMap: Map[String, Seq[String]]) extends AnyVal {
    def makeString: String = headerMap.map{case (k,v) => k -> v.mkString("[", ", ", "]")}.makeString
  }
}
