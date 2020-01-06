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
import java.util.UUID

import nl.knaw.dans.easy.deposit.PidRequester.PidType
import nl.knaw.dans.easy.deposit.PidRequester.PidType.PidType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.eclipse.jetty.http.HttpStatus.CREATED_201
import scalaj.http.{ BaseHttp, HttpResponse }

import scala.util.{ Failure, Success, Try }

class PidRequester(http: BaseHttp, uri: URI) extends DebugEnhancedLogging {

  def requestPid(id: UUID, pidType: PidType): Try[String] = Try {
    logger.info(s"[$id] calling easy-pid-generator with type ${ PidType.doi }")

    http(s"${ uri }create?type=$pidType")
      .timeout(connTimeoutMs = 10000, readTimeoutMs = 50000)
      .postForm
      .asString
  } flatMap {
    case HttpResponse(body, CREATED_201, _) =>
      logger.info(s"[$id] agreement generated successfully")
      Success(body)
    case HttpResponse(body, code, _) =>
      logger.info(s"[$id] PID generator failed with code $code and body $body")
      Failure(new RuntimeException(s"PID Generator failed with code $code and body $body"))
  }
}
object PidRequester {
  object PidType extends Enumeration {
    type PidType = Value
    val urn, doi = Value
  }
}
