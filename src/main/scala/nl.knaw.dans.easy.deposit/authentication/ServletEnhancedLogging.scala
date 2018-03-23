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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.ScalatraBase

trait ServletEnhancedLogging extends DebugEnhancedLogging {

  // TODO candidate for dans-scala-lib
  this: ScalatraBase =>

  before() {
    logger.info(s"${ request.getMethod } ${ request.getRequestURL } remote=${ request.getRemoteAddr } params=$params headers=${ request.headers } body=${ request.body }")
  }
  after() {
    //logger.info(s"response.status=${ response.getStatus } headers=${ response.headers }")
    // TODO would ignore the ActionResult of the servlets respond method, always causing a 200
    // see this fork (2011): https://github.com/erikrozendaal/scalatra#filters
    // it also got called before executing a servlet route with response == null
  }
}
