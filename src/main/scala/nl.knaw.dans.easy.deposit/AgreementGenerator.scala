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

import java.io.InputStream
import java.net.URL
import java.util.UUID

import nl.knaw.dans.easy.deposit.Errors.GeneratorError
import nl.knaw.dans.easy.deposit.docs.AgreementData
import nl.knaw.dans.easy.deposit.docs.JsonUtil._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.eclipse.jetty.http.HttpStatus.OK_200
import scalaj.http.BaseHttp

import scala.io.Source
import scala.util.{ Failure, Success, Try }

case class AgreementGenerator(http: BaseHttp, url: URL, acceptHeader: String) extends DebugEnhancedLogging {

  def generate(agreementData: AgreementData, id: UUID): Try[Array[Byte]] = {
    val json = toJson(agreementData)
    logger.info(s"calling easy-deposit-agreement-generator for $id with body: $json")
    Try(http(url.toString).postData(json)
      .header("content-type", "application/json")
      .header("accept", acceptHeader)
      .exec {
        case (OK_200, _, is) =>
          return Success(readAll(is))
        case (_, _, is) =>
          Source.fromInputStream(is).mkString
      }).flatMap { response =>
      Failure(GeneratorError(s"Could not generate agreement for dataset $id", response))
    }
  }

  private def readAll(is: InputStream) = {
    Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte).toArray
  }
}
