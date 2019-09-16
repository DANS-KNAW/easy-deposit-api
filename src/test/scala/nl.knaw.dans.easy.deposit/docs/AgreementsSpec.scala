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
import scala.xml.Elem

class AgreementsSpec extends TestSupportFixture {

  "apply" should "complain about not accepted deposit agreement" in {
    AgreementsXml(
      "user",
      DateTime.now,
      DatasetMetadata().copy(
        acceptDepositAgreement = false,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      userMap
    ) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage == "invalid DatasetMetadata: Please set AcceptDepositAgreement" =>
    }
  }
  it should "complain about not specifying the presence of privacy sensitive data" in {
    AgreementsXml(
      "user",
      DateTime.now,
      DatasetMetadata().copy(
        acceptDepositAgreement = true,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified
      ),
      userMap
    ) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage == "invalid DatasetMetadata: Please set PrivacySensitiveDataPresent" =>
    }
  }

  private lazy val triedSchema: Try[Schema] = AgreementsXml.loadSchema

  "schema validation" should "succeed with an empty full name" in {
    validate(AgreementsXml(
      "user",
      DateTime.now,
      DatasetMetadata().copy(
        acceptDepositAgreement = true,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      Map(
        "displayName" -> Seq(""),
        "email" -> Seq(""),
      )
    ))
  }

  it should "fail without an emal property for the user" in {
    validate(AgreementsXml(
      "user",
      DateTime.now,
      DatasetMetadata().copy(
        acceptDepositAgreement = true,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      Seq(
        "displayName" -> Seq(""),
      ).toMap
    ))
  }

  it should "succeed without a user" in {
    validate(AgreementsXml(
      null, // the attribute is omitted from <signerId easy-account={userId}>{fullname}</signerId>
      null, // the schema is happy with <dateSigned></dateSigned>
      DatasetMetadata().copy(
        acceptDepositAgreement = true,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      null, // the schema is happy with <signerId></signerId>
    ))
  }

  private def validate(triedXML: Try[Elem]) = {
    triedXML shouldBe a[Success[_]]
    assume(triedSchema.isAvailable)
    triedXML.flatMap(triedSchema.validate) shouldBe a[Success[_]]
  }
}
