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

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.{ ActionResult, ScalatraBase }

import scala.util.{ Failure, Success, Try }

// TODO candidate for dans-scala-lib / another package than authentication?
trait ServletEnhancedLogging extends DebugEnhancedLogging {
  this: ScalatraBase =>

  before() {
    // TODO a library version should filter any authentication any client might invent and probably provide hooks for more filters
    val headers = request.headers.map {
      case (key, value) if key.toLowerCase=="cookie" =>
        val cookieName = value.replaceAll("=.*", "")
        val cookieValue = value.replaceAll(".*=", "")
        val maskedCookieValue = cookieValue.replaceAll("[^.]", "*")
        (key, s"$cookieName=$maskedCookieValue")
      case (key, value) if key.toLowerCase.endsWith("authorization") =>
        // keep keys like "basic" and "digest" but mask the credentials
        (key, value.replaceAll(" .+", " *****"))
      case kv => kv
    }
    val maskedParams = params.map {
      case (key, _) if Seq("login", "password").contains(key.toLowerCase) => (key, "*****")
      case kv => kv
    }
    // https://www.bluecatnetworks.com/blog/ip-addresses-considered-personally-identifiable-information/
    // in case of link rot paste the url at the tail of https://web.archive.org/web/20181030102418/
    val maskedRemoteAddr = request.getRemoteAddr.replaceAll("[0-9]+[.][0-9]+$","**.**")
    logger.info(s"${ request.getMethod } ${ request.getRequestURL } remote=$maskedRemoteAddr; params=$maskedParams; headers=$headers")
  }
}
object ServletEnhancedLogging extends DebugEnhancedLogging {

  implicit class RichActionResult(val actionResult: ActionResult) extends AnyVal {
    /**
     * @example halt(BadRequest().logResponse)
     * @example Ok().logResponse
     * @return this
     */
    def logResponse(implicit request: HttpServletRequest, response: HttpServletResponse): ActionResult = {
      // Disadvantage of this method: developers might forget to call,
      // but an after method of the trait is not executed after a halt anyway.
      // Halt is typically used during authentication.
      // Another disadvantage of an after method: it might not have the response to log,
      // which should have been copied into "implicit response: HttpServletResponse".
      // See the last extensive readme version (documentation moved into an incomplete book and guides)
      // https://github.com/scalatra/scalatra/blob/6a614d17c38d19826467adcabf1dc746e3192dfc/README.markdown
      // sections #filters #action
      val authHeaders = response.getHeaderNames.toArray().map{
        case "Expires" => "Expires " + response.getHeader("Expires")
        case headerName => headerName
      }
      logger.info(s"${ request.getMethod } returned status=${ actionResult.status }; authHeaders=$authHeaders; actionHeaders=${ actionResult.headers }")
      actionResult
    }
  }

  implicit class RichTriedActionResult(val tried: Try[ActionResult]) extends AnyVal {
    // convenience replacing: .getOrRecover(???).logResponse
    // what happened to be the standard pattern within this project
    def getOrRecoverResponse(recover: Throwable => ActionResult)
                            (implicit request: HttpServletRequest, response: HttpServletResponse
                            ): ActionResult = {
      // the signature is more specific than for nl.knaw.dans.lib.error.TryExtensions.getOrRecover
      // it comes with the trait, not with just an import
      (tried match {
        case Success(actionResult) => actionResult
        case Failure(throwable) => recover(throwable)
      }).logResponse
    }
  }
}
