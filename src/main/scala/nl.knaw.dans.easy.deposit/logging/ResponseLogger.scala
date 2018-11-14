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
import org.scalatra.{ ActionResult, ScalatraBase }

trait AbstractResponseLogger {
  this: ScalatraBase with ResponseLogFormatter =>
  implicit val responseLogger: AbstractResponseLogger = this

  def logResponse(actionResult: ActionResult): Unit
}
trait ResponseLogger extends AbstractResponseLogger with DebugEnhancedLogging with ResponseLogFormatter {
  this: ScalatraBase =>
  override def logResponse(actionResult: ActionResult): Unit = {
    logger.info(formatResponseLog(actionResult))
  }
}
