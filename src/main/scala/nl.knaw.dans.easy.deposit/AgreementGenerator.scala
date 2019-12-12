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

import better.files.StringExtensions
import nl.knaw.dans.easy.deposit.docs.AgreementData
import scalaj.http.BaseHttp

import scala.util.{ Success, Try }
import nl.knaw.dans.easy.deposit.docs.JsonUtil._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

trait AgreementGenerator extends DebugEnhancedLogging {
  val http: BaseHttp
  val url: URL

  def agreementDoc(agreementData: AgreementData): Try[InputStream] = {
    val json = toJson(agreementData)
    logger.info(json)
    Success(s"not yet implemented $json".inputStream)
  }
}
