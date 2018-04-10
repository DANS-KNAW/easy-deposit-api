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
import nl.knaw.dans.easy.deposit.docs.Json.InvalidDocument

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
      |  "datesIso8601": [
      |    {
      |      "scheme": "string",
      |      "value": "string"
      |    }
      |  ],
      |  "dates": [
      |    {
      |      "scheme": "string",
      |      "value": "string"
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
      |  "extraClarinMetadataPresent": true,
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

  "deserialization/serialisation" should "at most have different white space and different order of fields" in {
    val expected = example.split("\n").map(_.trim.replaceAll(": ", ":")).mkString
    val result = Json.toJson(Json.getDatasetMetadata(example).getOrElse(""))
    //result shouldBe expected // might suffer from random order of fields, but can help debugging
    result.length shouldBe expected.length
  }

  it should "return defaults for omitted fields and omit null fields that don't have a default" in {
    val example =
      """{
        |  "creators": [
        |    {
        |      "insertions": null,
        |    }
        |  ]
        |}""".stripMargin
    Json.toJson(Json.getDatasetMetadata(example).getOrElse("")) shouldBe
      """{"creators":[{}],"extraClarinMetadataPresent":false,"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
  }

  "deserialization" should "ignore additional info" in {
    //JObject(List((x,JInt(1))))
    inside(Json.getDatasetMetadata("""{"x":1}""")) {
      case Success(_: DatasetMetadata) =>
    }
  }

  it should "extract just the last object" in {
    inside(Json.getDatasetMetadata("""{"languageOfDescription": "string"}{"doi": "doi:10.17632/DANS.6wg5xccnjd.1"}""")) {
      case Success(dm: DatasetMetadata) =>
        dm.doi shouldBe Some("doi:10.17632/DANS.6wg5xccnjd.1")
        dm.languageOfDescription shouldBe None
    }
  }

  it should "be happy with empty objects" in {
    // JObject(List())
    inside(Json.getDatasetMetadata("""{}{}""")) { case Success(_: DatasetMetadata) => }
  }

  it should "fail on an empty array" in {
    // JArray(List())
    inside(Json.getDatasetMetadata("""[]""")) { case Failure(_: InvalidDocument) => }
  }

  it should "not accept a literal number" in {
    inside(Json.getDatasetMetadata("""123""")) { case Failure(_: InvalidDocument) => }
  }
}
