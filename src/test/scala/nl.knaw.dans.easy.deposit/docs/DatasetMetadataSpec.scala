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

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.InvalidDocumentException
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.lib.error._
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.json4s.{ Diff, JsonInput }
import org.scalatest.Assertion

import scala.util.{ Failure, Success }

class DatasetMetadataSpec extends TestSupportFixture {

  implicit class RichString(str: String) {
    def causesInvalidDocumentException(expectedMessage: String): Assertion = {
      DatasetMetadata(str) should matchPattern {
        case Failure(e: InvalidDocumentException)
          if e.getMessage == expectedMessage =>
      }
    }
  }

  "deserialization/serialisation" should "produce the same json object structure" in {
    roundTripTest("datasetmetadata.json")
  }

  it should "not fail for UI test data (all fields)" in {
    // https://github.com/DANS-KNAW/easy-deposit-ui/blob/81b21a08ce1a8a3d86ef74148c1e024188080e10/src/test/typescript/mockserver/metadata.ts#L255-L585
    roundTripTest("datasetmetadata-from-ui-all.json")
  }

  it should "not fail for UI test data (some fields)" in {
    // https://github.com/DANS-KNAW/easy-deposit-ui/blob/f0d74722b36997389462b7bdd8191f5743aef45f/src/test/typescript/mockserver/metadata.ts#L597-L652
    // added:  "acceptDepositAgreement": false
    roundTripTest("datasetmetadata-from-ui-some.json")
  }

  private def roundTripTest(value: String): Unit = {
    val example = getManualTestResource(value)
    val parsed = prepareDatasetMetadata(example)
    val serializedObject = JsonMethods.parse(toJson(parsed))
    (JsonMethods.parse(example) diff serializedObject) should matchPattern {
      case Diff(JNothing, JNothing, JNothing) =>
    }
  }

  it should "return defaults for omitted mandatory fields" in {
    val example = """{"creators": [ ]}"""
    val parsed = prepareDatasetMetadata(example)
    toJson(parsed) shouldBe
      """{"creators":[],"privacySensitiveDataPresent":"unspecified","acceptDepositAgreement":false}"""
  }

  private def prepareDatasetMetadata(example: String): DatasetMetadata = {
    DatasetMetadata(example).getOrRecover(e => fail(e.toString, e))
  }

  "DatasetMetadata.dates" should "accept a date without a scheme" in {
    val s: JsonInput =
      """{"dates": [{ "value": "2018", "qualifier": "dcterms:created" }]}"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept a plain date" in {
    val s: JsonInput =
      """{"dates":[{"qualifier":"dcterms:date","scheme":"dcterms:W3CDTF","value":"2019-03-29T15:08:34+01:00"}]}"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  "DatasetMetadata.relations" should "accept complete relations" in {
    val s: JsonInput =
      """{
        |  "relations": [
        |    { "qualifier": "dcterms:hasFormat", "url": "http://host", "title": "string" },
        |    { "scheme": "id-type:URN", "value": "string", "qualifier": "dcterms:hasFormat" }
        |  ]
        |}""".stripMargin
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept incomplete relations" in {
    val s: JsonInput =
      """{
        |  "relations": [
        |    { "qualifier": "dcterms:hasFormat", "url": "http://host" },
        |    { "value": "string", "qualifier": "dcterms:hasFormat" }
        |  ]
        |}""".stripMargin
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept incomplete relations in a different order" in {
    val s =
      """{
        |  "relations": [
        |    { "value": "string", "qualifier": "dcterms:hasFormat" },
        |    { "qualifier": "dcterms:hasFormat", "url": "http://host" }
        |  ]
        |}""".stripMargin
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept a RelatedIdentifier without scheme" in {
    DatasetMetadata("""{ "relations": [ { "value": "string", "qualifier": "dcterms:hasFormat" } ] }""") shouldBe a[Success[_]]
  }

  it should "accept a Relation without title" in {
    DatasetMetadata("""{ "relations": [ { "qualifier": "dcterms:hasFormat", "url": "http://host" } ] }""") shouldBe a[Success[_]]
  }

  it should "accept a Relation without url" in {
    DatasetMetadata("""{ "relations": [ { "qualifier": "dcterms:hasFormat", "title": "string" } ] }""") shouldBe a[Success[_]]
  }

  it should "reject a RelatedIdentifier with an empty scheme" in {
    val s = """{ "relations": [ { "scheme": "", "value": "abc", "qualifier": "dcterms:hasFormat" } ] }"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  "DatasetMetadata.author" should "accept an author with initials and surname" in {
    DatasetMetadata(
      """{ "creators": [ {
        | "initials": "A",
        |  "surname": "Einstein",
        |  "role": {
        |        "scheme": "datacite:contributorType",
        |        "key": "RightsHolder",
        |        "value": "rightsholder"
        |      }
        |} ] }""".stripMargin)
      .map(_.rightsHolders.map(_.toString).mkString(";")) shouldBe Success("A Einstein")
  }

  it should "accept an organisation as author" in {
    DatasetMetadata("""{ "contributors": [ { "organization": "University of Zurich" } ] }""") shouldBe a[Success[_]]
  }

  "DatasetMetadata.audience" should "accept an audience without a scheme" in {
    DatasetMetadata("""{"audiences": [{ "key": "yyy", "value": "" }]}""") shouldBe a[Success[_]]
  }

  it should "accept both quoted and non quoted numbers" in {
    // the server will return all numbers quoted
    val s =
      """{"spatialBoxes": [ { "scheme": "RD", "north": "486890.5", "east": 121811.88, "south": 436172.5,  "west": 91232.016 }]}""".stripMargin
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  "DatasetMetadata.*" should "only report the item in the list that are invalid" in {
    val alteredData = createCorruptMetadataJsonString("\"scheme\": \"string\"", "\"invalid\": \"property\"")
    DatasetMetadata(alteredData) should matchPattern {
      case Failure(ide: InvalidDocumentException) if ide.getMessage.contains("{\"languageOfDescription\":{\"invalid\":\"property\"},\"audiences\":{\"invalid\":\"property\"},\"subjects\":{\"invalid\":\"property\"},\"languagesOfFiles\":{\"invalid\":\"property\"},\"temporalCoverages\":{\"invalid\":\"property\"}}") =>
    }
  }

  "DatasetMetadata.spatialBoxes" should "only report spatial points of the box that are invalid" in {
    val boxes: String =
      """{
        |	"spatialBoxes": [{
        |		"north-west": 2,
        |		"south-east": 3,
        |		"north": 4,
        |		"east": 5,
        |		"south": 9,
        |		"west": 10
        |	}],
        |}""".stripMargin
    DatasetMetadata(boxes) should matchPattern {
      case Failure(ide: InvalidDocumentException) if ide.getMessage == "invalid DatasetMetadata: don't recognize {\"spatialBoxes\":{\"north-west\":2,\"south-east\":3}}" =>
    }
  }

  "DatasetMetadata.spatialCoverages" should "only report someCoverage if is supplied instead of spatialCoverages" in {
    val alteredData = createCorruptMetadataJsonString(" \"spatialCoverages\"", " \"someCoverage\"")
    DatasetMetadata(alteredData) should matchPattern {
      case Failure(ide: InvalidDocumentException) if ide.getMessage == "invalid DatasetMetadata: don't recognize {\"someCoverage\":[{\"scheme\":\"dcterms:ISO3166\",\"value\":\"string\",\"key\":\"string\"}]}" =>
    }
  }

  "DatasetMetadata.Dates" should "only report the dates that are not correct" in {
    val dates =
      """{
        |	"dates": [{
        |			"scheme": "dcterms:W3CDTF",
        |			"value": "2018-05-31",
        |			"qualifier": "dcterms:created"
        |		},
        |		{
        |			"invalidOne": "invalid",
        |			"invalidValue": "2018-05-31",
        |			"invalidQualifier": "dcterms:created"
        |		}
        |	]
        |}""".stripMargin
    DatasetMetadata(dates) should matchPattern {
      case Failure(ide: InvalidDocumentException) if ide.getMessage == """invalid DatasetMetadata: don't recognize {"dates":{"invalidOne":"invalid","invalidValue":"2018-05-31","invalidQualifier":"dcterms:created"}}""" =>
    }
  }

  "DatasetMetadata.creators" should "only report the part of the creators that are wrong" in {
    val creatorValid = """{"titles": "Msc", "initials": "H.A.M.", "surname": "Boter", "ids": [ { "scheme": "DAI", "value":  "93313935x"}, {"scheme": "BSN", "value": "1234"} ], "organization" :"DANS"}"""
    val creatorInvalidIdElement = """{"titles": "Msc", "initials": "B.A.M.", "surname": "Hoter", "ids": [ { "scheme": "DAI", "value":  "93313935Z"}, {"scheme": "BSN", "value": "1235", "organization": "overheid"} ], "organization" :"DANS"}"""
    val creatorInvalidElementAtRootLevel = """{"titles": "Aartshertog", "FirstName": "jan-willem-hendrik", "surname": "Oranje", "ids": [ { "scheme": "DAI", "value":  "93313935y"}, {"scheme": "BSN", "value": "9999"} ], "organization" :"DANS"}"""
    //TODO moet hij hier niet struikelen over het veld role[0].waarde?
    val creatorInvalidRole =
      """{"titles": "Dr", "initials": "S.", "surname": "Pieterzoon", "ids": [ { "scheme": "DAI", "value":  "93313935i"} ], "organization" :"DANS", "role": [ { "scheme": "datacite:contributorType", "key": "ContactPerson", "waarde": "invalid", "andereWaarde": "invalid"} ] }"""
    val creators = s"""{ "creators": [$creatorValid, $creatorInvalidIdElement, $creatorInvalidElementAtRootLevel, $creatorInvalidRole] }"""
    DatasetMetadata(creators) should matchPattern {
      case Failure(ide: InvalidDocumentException) if ide.getMessage == """invalid DatasetMetadata: don't recognize {"creators":[{"ids":{"organization":"overheid"}},{"FirstName":"jan-willem-hendrik"}]}""" =>
    }
  }

  "DatasetMetadata.[creators, contributors]" should "only report the part of the creators and contributors that are wrong" in {
    val metaData =
      s"""{
         |	"contributors": [{
         |			"organization": "rightsHolder1",
         |			"role": {
         |				"scheme": "datacite:contributorType",
         |				"key": "RightsHolder",
         |				"value": "rightsholder"
         |			},
         |			"ids": [{
         |				"scheme": "aScheme",
         |				"key": "aKey",
         |				"value": "aValue",
         |				"waarde": "invalidProperty"
         |			}]
         |		},
         |		{
         |			"titles": "Dr.",
         |			"initials": "A.S.",
         |			"insertions": "van",
         |			"surname": "Terix",
         |			"role": {
         |				"scheme": "datacite:contributorType",
         |				"key": "RightsHolder",
         |				"value": "rightsholder",
         |				"otherKey": "placeHolder",
         |				"otherValue": "placeHolder"
         |			}
         |		}
         |	]
         |}""".stripMargin

    //TODO it also complains about the the field "key" in ids along with the invlaid field "waarde"
    val expectedErrorMsg =
      """invalid DatasetMetadata: don't recognize {"contributors":[{"ids":{"key":"aKey","waarde":"invalidProperty"}},{"role":{"otherKey":"placeHolder","otherValue":"placeHolder"}}]}""".stripMargin
    DatasetMetadata(metaData) should matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage == expectedErrorMsg =>
    }
  }

  private def createCorruptMetadataJsonString(pattern: String, replacement: String): String = {
    File("src/test/resources/manual-test/datasetmetadata.json").contentAsString.replaceAll(pattern, replacement)
  }
}
