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
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.AccessCategory.openForRegisteredUsers
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.AccessRights
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.dateSubmitted
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, RichJsonInput, toJson }
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.json4s.{ Diff, JsonInput }

import scala.util.{ Failure, Success }

class DatasetMetadataSpec extends TestSupportFixture {
  private val example =
    """{
      |  "identifiers": [
      |    {
      |      "scheme": "doi",
      |      "value": "10.17632/DANS.6wg5xccnjd.1"
      |    }
      |  ],
      |  "languageOfDescription": {
      |    "scheme": "string",
      |    "value": "string",
      |    "key": "string"
      |  },
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
      |  "audiences": [
      |    {
      |      "scheme": "string",
      |      "value": "string",
      |      "key": "string"
      |    }
      |  ],
      |  "subjects": [
      |    {
      |      "scheme": "string",
      |      "value": "string",
      |      "key": "string"
      |    }
      |  ],
      |  "alternativeIdentifiers": [
      |    {
      |      "scheme": "string",
      |      "value": "string"
      |    }
      |  ],
      |  "relations": [
      |    {
      |      "qualifier": "dcterms:hasFormat",
      |      "url": "string",
      |      "title": "string"
      |    },
      |    {
      |      "scheme": "string",
      |      "value": "string",
      |      "qualifier": "string"
      |    }
      |  ]
      |  "languagesOfFiles": [
      |    {
      |      "scheme": "string",
      |      "value": "string",
      |      "key": "string"
      |    }
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
      |  "publishers": [
      |    "string"
      |  ],
      |  "accessRights": {
      |    "category": "open",
      |    "group": "string"
      |  },
      |  "license": "string",
      |  "typesDcmi": [
      |    "string"
      |  ],
      |  "types": [
      |    {
      |      "scheme": "string",
      |      "value": "string"
      |    }
      |  ],
      |  "formats": [
      |    {
      |      "scheme": "string",
      |      "value": "string"
      |    }
      |  ],
      |  "temporalCoverages": [
      |    {
      |      "scheme": "string",
      |      "value": "string",
      |      "key": "string"
      |    }
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
      |  "spatialCoverages": [
      |    {
      |      "scheme": "string",
      |      "value": "string",
      |      "key": "string"
      |    }
      |  ],
      |  "messageForDataManager": "string",
      |  "privacySensitiveDataPresent": "yes",
      |  "acceptLicenseAgreement": true
      |}""".stripMargin

  "deserialization/serialisation" should "produce the same json object structure" in {
    val parsed = prepareDatasetMetadata(example)
    val serializedObject = JsonMethods.parse(toJson(parsed))
    inside(JsonMethods.parse(example) diff serializedObject) {
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
    val parsed = prepareDatasetMetadata(example)
    toJson(parsed) shouldBe
      """{"creators":[],"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
  }

  private def prepareDatasetMetadata(example: String) = {
    val tried = DatasetMetadata(example)
    tried.getOrElse(fail(s"preparation failed with: $tried"))
  }

  "deserialization" should "report additional json info" in {
    val example ="""{"titles":["foo bar"],"x":[1]}""".stripMargin
    inside(DatasetMetadata(example)) {
      case Failure(InvalidDocumentException(docName, t)) =>
        docName shouldBe "DatasetMetadata"
        t.getMessage shouldBe """don't recognize {"x":[1]}"""
    }
  }

  it should "extract just the last object" in {
    val s =
      """{"languageOfDescription": "string"}
        |{  "identifiers": [
        |    {
        |      "scheme": "doi",
        |      "value": "10.17632/DANS.6wg5xccnjd.1"
        |    }
        |  ]
        |}""".stripMargin
    inside(DatasetMetadata(s)) {
      case Success(dm: DatasetMetadata) =>
        dm.doi shouldBe Some("10.17632/DANS.6wg5xccnjd.1")
        dm.languageOfDescription shouldBe None
    }
  }

  it should "be happy with empty objects" in {
    // JObject(List())
    inside(DatasetMetadata("""{}{}""")) { case Success(_: DatasetMetadata) => }
  }

  it should "fail on an empty array" in {
    // JArray(List())
    inside(DatasetMetadata("""[]""")) { case Failure(InvalidDocumentException(_, t)) =>
      t.getMessage shouldBe "expected an object, got a class org.json4s.JsonAST$JArray"
    }
  }

  it should "not accept a literal number" in {
    inside(DatasetMetadata("""123""")) { case Failure(InvalidDocumentException(_, t)) =>
      t.getMessage shouldBe
        """expected field or array
          |Near: 12""".stripMargin
    }
  }

  "QualifiedDate" should "serialize with prefixed enum" in {
    val date = DatasetMetadata.Date(None, "2018-05-22", dateSubmitted)
    toJson(date) shouldBe """{"value":"2018-05-22","qualifier":"dcterms:dateSubmitted"}"""
  }

  it should "deserialize a prefixed enum" in {
    val s: JsonInput = """{"value":"2018-05-22","qualifier":"dcterms:dateSubmitted"}"""
    s.deserialize[DatasetMetadata.Date] shouldBe a[Success[_]]
  }

  "AccessCategory" should "serialize with prefix-less enum" in {
    toJson(AccessRights(openForRegisteredUsers, "")) shouldBe """{"category":"open_for_registered_users","group":""}"""
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
