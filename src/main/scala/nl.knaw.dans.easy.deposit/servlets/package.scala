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

import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.{ ActionResult, BadRequest, InternalServerError }

import scala.util.{ Failure, Success, Try }

package object servlets extends DebugEnhancedLogging {

  def internalErrorResponse(t: Throwable): ActionResult = {
    logger.error(s"Not expected exception: ${ t.getMessage }", t)
    InternalServerError("Internal Server Error")
  }

  def badDocResponse(t: InvalidDocumentException): ActionResult = {
    logger.error(t.getMessage)
    BadRequest(s"Bad Request. ${ t.getMessage }")
  }

  implicit class RichIterable[T](val xs: Stream[Try[T]]) extends AnyVal {
    def failFast: Try[Seq[T]] = {
      // TODO dans-lib candidate?
      val successes = Seq.newBuilder[T]

      xs.foreach {
        case Success(t) => successes += t
        case Failure(e) =>
          return Failure(e)
      }

      Success(successes.result())
    }
  }
}
