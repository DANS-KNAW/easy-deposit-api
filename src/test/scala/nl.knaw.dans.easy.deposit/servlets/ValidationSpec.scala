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
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalatest.Assertion
import org.scalatest.exceptions.TestFailedException
import org.xml.sax.SAXParseException

import scala.util.{ Failure, Success }

class ValidationSpec extends DepositServletFixture {

  "PUT(metadata) should succeed with incomplete authors, PUT(submitted)" should "fail for an empty creator" in {
    def checkSubmitResponse = {
      body should include("'dcx-dai:creatorDetails' is not complete")
      body shouldNot include("'dcx-dai:contributorDetails' is not complete")
      // a client has no clue there is a second violation on contributorDetails
      status shouldBe BAD_REQUEST_400
    }

    saveAndSubmit(checkSubmitResponse _, mandatoryOnSubmit.copy(
      creators = Some(Seq(Author())),
      contributors = Some(Seq(Author())),
    ))
  }

  it should "fail for an empty contributor" in {
    def checkSubmitResponse = {
      body should include("'dcx-dai:contributorDetails' is not complete")
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
    DDM(parse("""{ "creators": [ { "surname": "Einstein", "initials": "  " }]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains("'dcx-dai:author' is not complete")
          && cause.getLineNumber == 8 =>
      // TODO error handling should produce the XML line(s) to give the client a clue about the violating instance
    }
  }

  it should "fail for an author with just initials" in {
    DDM(parse("""{ "creators": [ { "surname": "  ", "initials": "A" }]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains("'dcx-dai:creatorDetails' is not complete") =>
    }
  }

  it should "fail without creators" in {
    DDM(parse("""{ "creators": []}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains("Invalid content was found starting with element 'ddm:created'") =>
    }
  }

  it should "fail for an ID without schema" in pendingUntilFixed {
    DDM(parse(
        """{ "creators": [ {
          |  "initials": "A",
          |  "surname": "Einstein",
          |  "ids": [{ "scheme": "id-type:ORCID" }]
          |}]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains(" is not complete") =>
    }
  }

  it should "fail for an ID without a value" in pendingUntilFixed {
    DDM(parse(
        """{ "creators": [ {
          |  "initials": "A",
          |  "surname": "Einstein",
          |  "ids": [{ "value": "0000-0002-9079-593X" }]
          |}]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains(" is not complete") =>
    }
  }

  it should "fail for a role without a key" in pendingUntilFixed {
    DDM(parse(
        """{ "creators": [
          |  { "role": {
          |      "scheme": "datacite:contributorType",
          |      "value": "Rights Holder"
          |    },
          |    "organization": "KNAW"
          |  }
          |]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains(" is not complete") =>
    }
  }

  it should "fail for a role without a value" in pendingUntilFixed {
    DDM(parse(
        """{ "creators": [
          |  { "role": {
          |      "scheme": "datacite:contributorType",
          |      "key": "RightsHolder"
          |    },
          |    "organization": "KNAW"
          |  }
          |]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains(" is not complete") =>
    }
  }

  it should "fail for a role without a scheme " in pendingUntilFixed {
    DDM(parse(
        """{ "creators": [
          |  { "role": {
          |      "key": "RightsHolder",
          |      "value": "Rights Holder"
          |    },
          |    "organization": "KNAW"
          |  }
          |]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains("'dcx-dai:creatorDetails' is not complete") =>
    }
  }

  it should "fail for a rightsHolding creator with neither surname nor organisation" in {
    DDM(parse(
        """{ "creators": [
          |  { "role": {
          |      "scheme": "datacite:contributorType",
          |      "key": "RightsHolder",
          |      "value": "Rights Holder"
          |    }
          |  }
          |]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains("'dcx-dai:creatorDetails' is not complete") =>
    }
  }


  it should "fail for a rightsHolding contributor with neither surname nor organisation" in {
    DDM(parse(
        """{ "contributors": [
          |  { "role": {
          |      "scheme": "datacite:contributorType",
          |      "key": "RightsHolder",
          |      "value": "Rights Holder"
          |    }
          |  }
          |]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: SAXParseException))
        if cause.getMessage.contains("'dcx-dai:contributorDetails' is not complete") =>
    }
  }

  it should "fail for organisations with insertions and/or titles" in {
    DDM(parse(
        """{ "creators": [
          |  { "titles": "Baron", "insertions": "van", "organization": "Nyenrode" },
          |  { "organization": "Harvard", "insertions": "  ", "surname": "  " }
          |  { "titles": "Sir", "organization": "Oxbridge" }
          |  { "titles": "Mr" }
          |  { "insertions": "von", "organization": "ETH Zurich" },
          |]}""")
    ) should matchPattern {
      case Failure(InvalidDocumentException("DatasetMetadata", cause: IllegalArgumentException))
        if cause.getMessage == """An author without surname should have neither titles nor insertions, got: {"titles":"Baron","insertions":"van","organization":"Nyenrode"}, {"titles":"Sir","organization":"Oxbridge"}, {"titles":"Mr"}, {"insertions":"von","organization":"ETH Zurich"}""" =>
    }
  }

  "PUT(metadata) and PUT(submitted)" should "succeed for the base object for each test" in {
    // more valid data is tested in other test classes with src/test/resources/manual-test/*.json
    DDM(mandatoryOnSubmit) shouldBe a[Success[_]]
  }

  /**
   * @param input json object with metadata fragment under test
   * @return public option fields of parsed input injected into a valid-for-submission instance
   * @throws TestFailedException when the input can't be parsed
   *                             and thus would be rejected by PUT(metadata).
   */
  private def parse(input: String): DatasetMetadata = {
    val parsed = DatasetMetadata(input.stripMargin)
      .getOrRecover(e => fail(s"loading test data failed: ${ e.getMessage }; $input", e))

    mandatoryOnSubmit.copy(
      // (alternative)identifiers and types(DCMI) are private and thus not injectable
      // but other fields have the same types to test validation
      languageOfDescription = parsed.languageOfDescription orElse mandatoryOnSubmit.languageOfDescription,
      titles = parsed.titles orElse mandatoryOnSubmit.titles,
      alternativeTitles = parsed.alternativeTitles orElse mandatoryOnSubmit.alternativeTitles,
      descriptions = parsed.descriptions orElse mandatoryOnSubmit.descriptions,
      creators = parsed.creators orElse mandatoryOnSubmit.creators,
      contributors = parsed.contributors orElse mandatoryOnSubmit.contributors,
      audiences = parsed.audiences orElse mandatoryOnSubmit.audiences,
      subjects = parsed.subjects orElse mandatoryOnSubmit.subjects,
      relations = parsed.relations orElse mandatoryOnSubmit.relations,
      languagesOfFiles = parsed.languagesOfFiles orElse mandatoryOnSubmit.languagesOfFiles,
      // TODO dates are private and thus not injectable for testing
      sources = parsed.sources orElse mandatoryOnSubmit.sources,
      instructionsForReuse = parsed.instructionsForReuse orElse mandatoryOnSubmit.instructionsForReuse,
      publishers = parsed.publishers orElse mandatoryOnSubmit.publishers,
      accessRights = parsed.accessRights orElse mandatoryOnSubmit.accessRights,
      license = parsed.license orElse mandatoryOnSubmit.license,
      formats = parsed.formats orElse mandatoryOnSubmit.formats,
      temporalCoverages = parsed.temporalCoverages orElse mandatoryOnSubmit.temporalCoverages,
      spatialPoints = parsed.spatialPoints orElse mandatoryOnSubmit.spatialPoints,
      spatialBoxes = parsed.spatialBoxes orElse mandatoryOnSubmit.spatialBoxes,
      spatialCoverages = parsed.spatialCoverages orElse mandatoryOnSubmit.spatialCoverages,
      messageForDataManager = parsed.messageForDataManager orElse mandatoryOnSubmit.messageForDataManager,
      // privacySensitiveDataPresent and acceptDepositAgreement are not options and thus not injected
    )
  }

  /** Tests inject parsed input into this object to check and document error reporting. */
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
