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
import nl.knaw.dans.lib.error._
import org.joda.time.DateTime

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, PrettyPrinter }

class AgreementsSpec extends TestSupportFixture {
  private val printer = new PrettyPrinter(160, 2)

  private def stripRootAttributes(xml: Elem) = { // their order is undefined
    printer.format(xml).replaceAll("""<agreements[^>]*>""", "<agreements>")
  }

  "apply" should "have the submitters email" in {
    val datasetMetadata: DatasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
      .getOrRecover(e => fail("could not get test input", e))

    val now = DateTime.now
    AgreementsXml(
      dateSubmitted = now,
      dm = datasetMetadata,
      user = UserData(
        "foo", name = "bar", email = "does.not.exist@dans.knaw.nl",
        firstName = None, prefix = None, lastName = "", phone = "",
        organisation = "", address = "", zipcode = "", city = "", country = "",
      ),
    ).map(stripRootAttributes) shouldBe Success(
      """<agreements>
        |  <depositAgreement>
        |    <signerId easy-account="foo" email="does.not.exist@dans.knaw.nl">bar</signerId>
        |    <dcterms:dateAccepted>2018-03-22T21:43:01.000+01:00</dcterms:dateAccepted>
        |    <depositAgreementAccepted>true</depositAgreementAccepted>
        |  </depositAgreement>
        |  <personalDataStatement>
        |    <signerId easy-account="foo" email="does.not.exist@dans.knaw.nl">bar</signerId>
        |    <dateSigned>2018-03-22T21:43:01.000+01:00</dateSigned>
        |    <containsPrivacySensitiveData>false</containsPrivacySensitiveData>
        |  </personalDataStatement>
        |</agreements>
        |""".stripMargin
    )
  }

  it should "not stumble over missing user properties" in {
    val datasetMetadata: DatasetMetadata = DatasetMetadata(getManualTestResource("datasetmetadata-from-ui-all.json"))
      .getOrRecover(e => fail("could not get test input", e))

    val now = DateTime.now
    AgreementsXml(
      dateSubmitted = now,
      dm = datasetMetadata,
      user = UserData(
        "foo", name = "", email = null, // whatever an AuthenticationProvider may throw at us
        firstName = None, prefix = None, lastName = "", phone = "",
        organisation = "", address = "", zipcode = "", city = "", country = "",
      ),
    ).map(stripRootAttributes) should matchPattern {
      case Success(s: String) if s.contains("""<signerId easy-account="foo">foo</signerId>""") =>
    }
  }

  it should "complain about not accepted deposit agreement" in {
    AgreementsXml(
      dateSubmitted = DateTime.now,
      dm = DatasetMetadata().copy(
        acceptDepositAgreement = false,
        privacySensitiveDataPresent = PrivacySensitiveDataPresent.no
      ),
      user = defaultUserInfo,
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
      user = defaultUserInfo,
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
      user = null,
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
      user = UserData(Map[String, Seq[String]]()),
    )
    triedXML shouldBe a[Success[_]]
    val triedSchema: Try[Schema] = AgreementsXml.loadSchema
    assume(triedSchema.isAvailable)
    triedXML.flatMap(triedSchema.validate) shouldBe a[Success[_]]
  }
}
