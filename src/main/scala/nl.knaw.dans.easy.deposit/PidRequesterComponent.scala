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

import java.net.URI

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

trait PidRequesterComponent extends DebugEnhancedLogging {

  val pidRequester: PidRequester
}
object PidRequesterComponent {

  trait PidRequester {
    this: HttpContext =>

    val pidGeneratorService: URI

    def requestPid(pidType: PidType): Try[String] = Try {
      Http(s"${ pidGeneratorService }create?type=$pidType")
        .timeout(connTimeoutMs = 10000, readTimeoutMs = 50000)
        .postForm
        .asString
    } flatMap {
      case r if r.code == 201 => Success(r.body)
      case r => Failure(new RuntimeException(s"PID Generator failed: code=${ r.code } body=${ r.body }"))
    }
  }
  object PidType extends Enumeration {
    type PidType = Value
    val urn, doi = Value
  }
}
