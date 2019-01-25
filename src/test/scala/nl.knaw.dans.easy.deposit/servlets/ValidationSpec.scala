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
package nl.knaw.dans.easy.deposit.servlets

import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.easy.deposit.docs._
import nl.knaw.dans.easy.deposit.docs.dm.Author
import org.eclipse.jetty.http.HttpStatus._
import org.json4s.JsonInput
import nl.knaw.dans.easy.deposit.docs.JsonUtil._
import org.scalatest.Assertion
import nl.knaw.dans.lib.error._
import org.xml.sax.SAXParseException

import scala.util.Failure

class ValidationSpec extends DepositServletFixture {

  "PUT(metadata) should succeed with incomplete authors, PUT(submitted)" should "fail for an empty creator" in {
    def checkSubmitResponse = {
      body should include("The content of element 'dcx-dai:creatorDetails' is not complete")
      body shouldNot include("The content of element 'dcx-dai:contributorDetails' is not complete")
      // a client has no clue about the second violation on contributorDetails
      status shouldBe BAD_REQUEST_400
    }

    saveAndSubmit(checkSubmitResponse _, mandatoryOnSubmit.copy(
      creators = Some(Seq(Author())),
      contributors = Some(Seq(Author())),
    ))
  }

  it should "fail for an empty contributor" in {
    def checkSubmitResponse = {
      body should include("The content of element 'dcx-dai:contributorDetails' is not complete")
      // a client has no clue which one in the list is violating the requirements
      status shouldBe BAD_REQUEST_400
    }

    saveAndSubmit(checkSubmitResponse _, mandatoryOnSubmit.copy(
      contributors = Some(Seq(
        Author(initials = Some("A.S."), surname = Some("Terix")),
        Author(),
      )),
    ))
  }

  // So far full round trip tests. They fire four requests each, in turn creating/updating files.
  // The following tests belong to the same theme of this test class,
  // but are reduced to the essence for faster execution.
  // Parsing json is part of PUT(metadata), creating a DDM object is part of PUT(submitted).

  it should "fail for an author with just a last name" in {
    DDM(mandatoryOnSubmit.copy(
      creators = Some(Seq(parseAuthor("""{ "surname": "Einstein" }""")))
    )) should matchPattern {
      case Failure(InvalidDocumentException(_, cause: SAXParseException))
        if cause.getMessage.contains("The content of element 'dcx-dai:author' is not complete")
        && cause.getLineNumber == 8 =>
      // TODO error handling should produce the XML line(s) to give the client a clue about the violating instance
    }
  }

  it should "fail for an author with just initials" in {
    DDM(mandatoryOnSubmit.copy(
      creators = Some(Seq(parseAuthor("""{ "initials": "A" }""")))
    )) should matchPattern {
      case Failure(InvalidDocumentException(_, cause: Throwable))
        if cause.getMessage.contains("The content of element 'dcx-dai:creatorDetails' is not complete") =>
    }
  }

  it should "fail for an organisation with insertions" in pendingUntilFixed { // TODO fix schema?
    DDM(mandatoryOnSubmit.copy(
      creators = Some(Seq(parseAuthor("""{ "insertions": "von", "organization": "ETH Zurich" }""")))
    )) should matchPattern {
      case Failure(InvalidDocumentException(_, cause: Throwable))
        if cause.getMessage.contains("The content of element 'dcx-dai:creatorDetails' is not complete") =>
    }
  }

  it should "fail for an organisation with titles" in pendingUntilFixed {
    DDM(mandatoryOnSubmit.copy(
      creators = Some(Seq(parseAuthor("""{ "titles": "Sir", "organization": "Oxbridge" }""")))
    )) should matchPattern {
      case Failure(InvalidDocumentException(_, cause: Throwable))
        if cause.getMessage.contains("The content of element 'dcx-dai:creatorDetails' is not complete") =>
    }
  }

  def parseAuthor(input: JsonInput): Author = input
    .deserialize[Author]
    .getOrRecover(e => fail(s"loading test data failed: ${e.getMessage}; $input", e))

  private val mandatoryOnSubmit = DatasetMetadata(
    """{
      |  "titles": ["blabla"],
      |  "descriptions": ["rababera"],
      |  "creators": [ { "initials": "A", "surname": "Einstein", } ],
      |  "dates": [
      |    { "scheme": "dcterms:W3CDTF", "value": "2018-05-31", "qualifier": "dcterms:available" },
      |    { "scheme": "dcterms:W3CDTF", "value": "2018-05-31", "qualifier": "dcterms:created" },
      |  ],
      |  "audiences": [ { "scheme": "string", "value": "string", "key": "D33000" } ],
      |  "accessRights": {"category": "OPEN_ACCESS"},
      |  "privacySensitiveDataPresent": "no",
      |  "acceptDepositAgreement": true,
      |}""".stripMargin
  ).getOrElse(fail("loading mandatory test data failed"))

  private def saveAndSubmit(checkSubmitResponse: () => Assertion,
                            metadata: DatasetMetadata
                           ) = {
    val uuid = createDeposit

    put(
      uri = s"/deposit/$uuid/metadata",
      headers = Seq(fooBarBasicAuthHeader),
      body = JsonUtil.toJson(metadata)
    ) { status shouldBe NO_CONTENT_204 }

    mockDoiRequest("10.17632/DANS.6wg5xccnjd.1") once()
    get( // added to metadata.json and deposit.properties
      // note that a client would need this DOI in the metadata for each next put
      uri = s"/deposit/$uuid/doi",
      headers = Seq(fooBarBasicAuthHeader)
    ) { status shouldBe OK_200 }

    assume(DDM.triedSchema.isAvailable)
    put(
      uri = s"/deposit/$uuid/state",
      headers = Seq(fooBarBasicAuthHeader),
      body = """{"state":"SUBMITTED","stateDescription":"blabla"}"""
    ) {
      checkSubmitResponse()
    }
  }
}
