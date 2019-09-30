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

import javax.xml.validation.Schema
import nl.knaw.dans.easy.deposit.Errors.InvalidDocumentException
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.dm.PrivacySensitiveDataPresent
import org.joda.time.DateTime

import scala.util.{ Failure, Success, Try }

class AgreementsSpec extends TestSupportFixture {

  "apply" should "complain about not accepted deposit agreement" in {
    AgreementsXml(
      dateSubmitted = DateTime.now,
      dm = DatasetMetadata().copy(
        acceptDepositAgreement = false,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      userInfo = defaultUserInfo,
    ) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage == "invalid DatasetMetadata: Please set AcceptDepositAgreement" =>
    }
  }

  it should "complain about not specifying the presence of privacy sensitive data" in {
    AgreementsXml(
      dateSubmitted = DateTime.now,
      dm = DatasetMetadata().copy(
        acceptDepositAgreement = true,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified
      ),
      userInfo = defaultUserInfo,
    ) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage == "invalid DatasetMetadata: Please set PrivacySensitiveDataPresent" =>
    }
  }

  it should "fail without user properties" in {
    AgreementsXml(
      dateSubmitted = null,
      dm = DatasetMetadata().copy(
        acceptDepositAgreement = true,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      userInfo = null,
    ) should matchPattern {
      case Failure(e: NullPointerException) =>
    }
  }

  "schema validation" should "succeed with missing user attributes" in {
    val triedXML = AgreementsXml(
      dateSubmitted = DateTime.now,
      dm = DatasetMetadata().copy(
        acceptDepositAgreement = true,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      userInfo = UserInfo(Map[String, Seq[String]]()),
    )
    triedXML shouldBe a[Success[_]]
    val triedSchema: Try[Schema] = AgreementsXml.loadSchema
    assume(triedSchema.isAvailable)
    triedXML.flatMap(triedSchema.validate) shouldBe a[Success[_]]
  }
}
