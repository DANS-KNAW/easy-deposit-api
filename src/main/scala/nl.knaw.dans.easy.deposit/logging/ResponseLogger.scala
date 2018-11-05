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
 * Enables logging of ActionResult objects.
 *
 * @example
 * {{{
 *   // enable the log method, any customazation goes here
 *
 *   package object servlets extends ResponseLogger {
 *     implicit class RichActionResult(val actionResult: ActionResult) extends AnyVal {
 *
 *       def logResponse(implicit request: HttpServletRequest, response: HttpServletResponse): ActionResult = {
 *         logActionResult(actionResult)
 *       }
 *     }
 *   }
 * }}}
 *
 * @example
 * {{{
 *   // usage in normal servlets
 *
 *   ... extends ScalatraServlet {
 *     get(???) {
 *       ???
 *       Ok(body = "hello world").logResponse
 *     }
 *   }
 * }}}
 *
 * @example
 * {{{
 *   // usage in authentication servlets
 *
 *   halt(Unauthorized(???).logResponse)
 * }}}
 */
trait ResponseLogger extends DebugEnhancedLogging {
  // When using after methods for logging, the action result of the second example above is lost,
  // its values are not saved in the implicit response as for authentication servlets built with AuthenticationSupport
  // See the last extensive readme version (documentation moved into an incomplete book and guides)
  // https://github.com/scalatra/scalatra/blob/6a614d17c38d19826467adcabf1dc746e3192dfc/README.markdown
  // sections #filters #action

  def logActionResult(actionResult: ActionResult)(implicit request: HttpServletRequest, response: HttpServletResponse): ActionResult = {
    // Disadvantage of this method: developers might forget to call,
    // but an after method of the trait is not executed after a halt anyway.
    val authHeaders = response.getHeaderNames.toArray().map {
      case "Expires" => "Expires " + response.getHeader("Expires")
      case headerName => headerName
    }
    logger.info(s"${ request.getMethod } returned status=${ actionResult.status }; authHeaders=$authHeaders; actionHeaders=${ actionResult.headers }")
    actionResult
  }
}
