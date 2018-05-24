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

import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.AccessCategory.open_for_registered_users
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.dateSubmitted
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ AccessRights, QualifiedDate }
import nl.knaw.dans.easy.deposit.docs.Json.{ InvalidDocumentException, getDatasetMetadata, toJson }
import org.json4s.{ Diff, JsonInput }
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import nl.knaw.dans.easy.deposit.docs.Json.RichJsonInput

import scala.util.{ Failure, Success }

class DatasetMetadataSpec extends TestSupportFixture {
  private val example =
    """{
      |  "doi": "doi:10.17632/DANS.6wg5xccnjd.1",
      |  "languageOfDescription": "string",
      |  "titles": [
      |    "Title 1",
      |    "Title 2"
      |  ],
      |  "alternativeTitles": [
      |    "string"
      |  ],
      |  "descriptions": [
      |    "string"
      |  ],
      |  "creators": [
      |    {
      |      "titles": "Prof Dr",
      |      "initials": "A",
      |      "surname": "Einstein",
      |      "ids": [
      |        {
      |          "scheme": "DOI",
      |          "value": "1234/5678"
      |        },
      |        {
      |          "scheme": "ISNI",
      |          "value": "ISNI|000000012281955X"
      |        }
      |      ],
      |      "organization": "University of Zurich"
      |    }
      |  ],
      |  "contributors": [
      |    {
      |      "titles": "Prof Dr",
      |      "initials": "A",
      |      "surname": "Einstein",
      |      "ids": [
      |        {
      |          "scheme": "DOI",
      |          "value": "1234/5678"
      |        },
      |        {
      |          "scheme": "ISNI",
      |          "value": "ISNI|000000012281955X"
      |        }
      |      ],
      |      "organization": "University of Zurich"
      |    }
      |  ],
      |  "dateCreated": "string",
      |  "audiences": [
      |    "string"
      |  ],
      |  "subjects": [
      |    "string"
      |  ],
      |  "identifiers": [
      |    {
      |      "scheme": "string",
      |      "value": "string"
      |    }
      |  ],
      |  "relations": [
      |    {
      |      "qualifier": "string",
      |      "url": "string",
      |      "title": "string"
      |    }
      |  ],
      |  "languagesOfFilesIso639": [
      |    "string"
      |  ],
      |  "languagesOfFiles": [
      |    "string"
      |  ],
      |  "dates": [
      |    {
      |      "scheme": "string",
      |      "value": "string",
      |      "qualifier": "dcterms:created"
      |    }
      |  ],
      |  "sources": [
      |    "string"
      |  ],
      |  "instructionsForReuse": [
      |    "string"
      |  ],
      |  "rightsHolders": [
      |    "string"
      |  ],
      |  "publishers": [
      |    "string"
      |  ],
      |  "accessRights": {
      |    "category": "open",
      |    "group": "string"
      |  },
      |  "license": "string",
      |  "dateAvailable": "string",
      |  "typesDcmi": [
      |    "string"
      |  ],
      |  "types": [
      |    "string"
      |  ],
      |  "formatsMediaType": [
      |    "string"
      |  ],
      |  "formats": [
      |    "string"
      |  ],
      |  "archisNrs": [
      |    "string"
      |  ],
      |  "subjectsAbrComplex": [
      |    "string"
      |  ],
      |  "temporalCoveragesAbr": [
      |    "string"
      |  ],
      |  "temporalCoverages": [
      |    "string"
      |  ],
      |  "spatialPoints": [
      |    {
      |      "scheme": "string",
      |      "x": 0,
      |      "y": 0
      |    }
      |  ],
      |  "spatialBoxes": [
      |    {
      |      "scheme": "string",
      |      "north": 0,
      |      "east": 0,
      |      "south": 0,
      |      "west": 0
      |    }
      |  ],
      |  "spatialCoverageIso3166": [
      |    {
      |      "scheme": "string",
      |      "value": "string"
      |    }
      |  ],
      |  "spatialCoverages": [
      |    "string"
      |  ],
      |  "messageForDataManager": "string",
      |  "privacySensitiveDataPresent": "yes",
      |  "acceptLicenseAgreement": true
      |}""".stripMargin

  "deserialization/serialisation" should "produce the same json object structure" in {
    val parsed = getDatasetMetadata(example).getOrElse("")
    inside(JsonMethods.parse(example) diff JsonMethods.parse(toJson(parsed))) {
      case Diff(JNothing, JNothing, JNothing) =>
      case x => fail(s"did not expect $x")
    }
  }

  it should "return defaults for omitted fields" in {
    val example =
      """{
        |  "creators": [
        |  ]
        |}""".stripMargin
    toJson(getDatasetMetadata(example).getOrElse("")) shouldBe
      """{"creators":[],"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
  }

  "deserialization" should "report additional json info" in {
    val example ="""{"titles":["foo bar"],"x":[1]}""".stripMargin
    inside(getDatasetMetadata(example)) {
      case Failure(InvalidDocumentException(docName, t)) =>
        docName shouldBe "DatasetMetadata"
        t.getMessage shouldBe """don't recognize {"x":[1]}"""
    }
  }

  it should "extract just the last object" in {
    inside(getDatasetMetadata("""{"languageOfDescription": "string"}{"doi": "doi:10.17632/DANS.6wg5xccnjd.1"}""")) {
      case Success(dm: DatasetMetadata) =>
        dm.doi shouldBe Some("doi:10.17632/DANS.6wg5xccnjd.1")
        dm.languageOfDescription shouldBe None
    }
  }

  it should "be happy with empty objects" in {
    // JObject(List())
    inside(getDatasetMetadata("""{}{}""")) { case Success(_: DatasetMetadata) => }
  }

  it should "fail on an empty array" in {
    // JArray(List())
    inside(getDatasetMetadata("""[]""")) { case Failure(InvalidDocumentException(_, t)) =>
      t.getMessage shouldBe "expected an object, got a class org.json4s.JsonAST$JArray"
    }
  }

  it should "not accept a literal number" in {
    inside(getDatasetMetadata("""123""")) { case Failure(InvalidDocumentException(_, t)) =>
      t.getMessage shouldBe
        """expected field or array
          |Near: 12""".stripMargin
    }
  }

  "QualifiedDate" should "serialize with prefixed enum" in {
    val date = QualifiedDate(None, "2018-05-22", dateSubmitted)
    toJson(date) shouldBe """{"value":"2018-05-22","qualifier":"dcterms:dateSubmitted"}"""
  }

  it should "deserialize a prefixed enum" in {
    val s: JsonInput = """{"value":"2018-05-22","qualifier":"dcterms:dateSubmitted"}"""
    s.deserialize[QualifiedDate] shouldBe a[Success[_]]
  }

  "AccessCategory" should "serialize with prefix-less enum" in {
    toJson(AccessRights(open_for_registered_users, "")) shouldBe """{"category":"open_for_registered_users","group":""}"""
  }
  it should "deserialize a prefix-less enum" in {
    val s: JsonInput = """{"category":"open_for_registered_users","group":""}"""
    s.deserialize[AccessRights] shouldBe a[Success[_]]
  }
  it should "refeuse to deserialize a prefix on a non-prefixed enum" in {
    val s: JsonInput = """{"category":"dcterms:open_for_registered_users","group":""}"""
    inside(s.deserialize[AccessRights]) {
      case Failure(InvalidDocumentException(msg, cause)) =>
        msg shouldBe "AccessRights"
        cause.getMessage should startWith("No usable value for")
        cause.getMessage should include("category")
    }
  }
}
