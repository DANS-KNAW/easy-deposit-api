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

import org.joda.time.DateTime

import scala.util.Try
import scala.xml.Elem

object AgreementsXml extends SchemedXml {
  override protected val schemaNameSpace = "http://easy.dans.knaw.nl/schemas/bag/metadata/agreements/"
  override protected val schemaLocation = "https://easy.dans.knaw.nl/schemas/bag/metadata/agreements/2018/05/agreements.xsd"

  def apply(userId: String, dateSubmitted: DateTime, dm: DatasetMetadata): Try[Elem] = {
    for {
      _ <- dm.depositAgreementAccepted
      privacy <- dm.hasPrivacySensitiveData
    } yield
      <agreements
          xmlns={schemaNameSpace}
          xmlns:dcterms="http://purl.org/dc/terms/"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation={s"$schemaNameSpace $schemaLocation"}>
        <licenseAgreement>
          <depositorId>{userId}</depositorId>
          <dateAccepted>{dateSubmitted}</dateAccepted>
          <licenseAgreementAccepted>{dm.acceptDepositAgreement}</licenseAgreementAccepted>
        </licenseAgreement>
        <personalDataStatement>
          <signerId>{userId}</signerId>
          <dateSigned>{dateSubmitted}</dateSigned>
          <containsPrivacySensitiveData>{privacy}</containsPrivacySensitiveData>
        </personalDataStatement>
      </agreements>
  }
}
