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
package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.docs.JsonUtil.RichJsonInput
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.JsonInput

import scala.util.Try

case class AgreementUser(name: String,
                         address: String,
                         zipcode: String,
                         city: String,
                         country: String,
                         organisation: String,
                         phone: String,
                         email: String,
                   )
object AgreementUser extends DebugEnhancedLogging {
  def apply(input: JsonInput): Try[UserInfo] = input.deserialize[UserInfo]

  def apply(data: UserData): AgreementUser = {
    new AgreementUser(
      name = data.name,
      address = data.address,
      zipcode = data.zipcode,
      city = data.city,
      country = data.country,
      organisation = data.organisation,
      phone = data.phone,
      email = data.email,
    )
  }
}
