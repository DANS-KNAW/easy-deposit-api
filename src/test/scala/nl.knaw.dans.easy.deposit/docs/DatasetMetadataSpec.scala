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
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, toJson }
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.json4s.{ Diff, JsonInput }

import scala.util.{ Failure, Success }

class DatasetMetadataSpec extends TestSupportFixture {

  "deserialization/serialisation" should "produce the same json object structure" in {
    roundTripTest("datasetmetadata.json")
  }

  it should "not fail for UI test data (all fields)" in {
    // https://github.com/DANS-KNAW/easy-deposit-ui/blob/81b21a08ce1a8a3d86ef74148c1e024188080e10/src/test/typescript/mockserver/metadata.ts#L255-L585
    roundTripTest("datasetmetadata-from-ui-all.json")
  }

  it should "not fail for UI test data (some fields)" in {
    // https://github.com/DANS-KNAW/easy-deposit-ui/blob/81b21a08ce1a8a3d86ef74148c1e024188080e10/src/test/typescript/mockserver/metadata.ts#L586-L642
    roundTripTest("datasetmetadata-from-ui-some.json")
  }

  private def roundTripTest(value: String): Unit = {
    val example = (File("src") / "test" / "resources" / "manual-test" / value).contentAsString
    val parsed = prepareDatasetMetadata(example)
    val serializedObject = JsonMethods.parse(toJson(parsed))
    inside(JsonMethods.parse(example) diff serializedObject) {
      case Diff(JNothing, JNothing, JNothing) =>
      case Diff(changed, added, deleted) => reportFailure(parsed, changed, added, deleted)
    }
  }

  private def reportFailure(parsed: DatasetMetadata, changed: JValue, added: JValue, deleted: JValue) = {
    fail(s"serialized parsed object not equal to original json object: changed=[$changed] added=[$added] deleted=[$deleted]; re-serialized json=[${ toJson(parsed) }]")
  }

  it should "return defaults for omitted mandatory fields" in {
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
        |      "scheme": "id-type:DOI",
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
    // this is not desired behaviour but is documenting what actually happens
    DatasetMetadata("""{}{}""") shouldBe a[Success[_]]
  }

  it should "fail on an empty array" in {
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

  "DatasetMetadata.relations" should "accept complete relations" in {
    val s: JsonInput =
      """{
        |  "relations": [
        |    { "qualifier": "dcterms:hasFormat", "url": "string", "title": "string" },
        |    { "scheme": "id-type:URN", "value": "string", "qualifier": "dcterms:hasFormat" }
        |  ]
        |}""".stripMargin
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept incomplete relations" in {
    val s: JsonInput =
      """{
        |  "relations": [
        |    { "qualifier": "dcterms:hasFormat", "url": "string" },
        |    { "value": "string", "qualifier": "dcterms:hasFormat" }
        |  ]
        |}""".stripMargin
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept incomplete relations in a different order" in {
    val s: JsonInput =
      """{
        |  "relations": [
        |    { "value": "string", "qualifier": "dcterms:hasFormat" },
        |    { "qualifier": "dcterms:hasFormat", "url": "string" }
        |  ]
        |}""".stripMargin
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept a RelatedIdentifier without scheme" in {
    val s: JsonInput = """{ "relations": [ { "value": "string", "qualifier": "dcterms:hasFormat" } ] }"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept a Relation without title" in {
    val s: JsonInput = """{ "relations": [ { "qualifier": "dcterms:hasFormat", "url": "string" } ] }"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept a Relation without url" in {
    val s: JsonInput = """{ "relations": [ { "qualifier": "dcterms:hasFormat", "title": "string" } ] }"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "reject a relation with just a qualifier" in {
    val s: JsonInput = """{ "relations": [ { "qualifier": "dcterms:hasFormat" } ] }"""
    shouldReturnCustomMessage(s, """expected one of (Relation | RelatedIdentifier) got: {"qualifier":"dcterms:hasFormat"}""")
  }

  "DatasetMetadata.Author" should "accept an author with initials and surname" in {
    val s: JsonInput = """{ "creators": [ { "initials": "A", "surname": "Einstein" } ] }"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "accept an organisation as author" in {
    val s: JsonInput = """{ "contributors": [ { "organization": "University of Zurich" } ] }"""
    DatasetMetadata(s) shouldBe a[Success[_]]
  }

  it should "reject an author without initials" in {
    val s: JsonInput = """{ "contributors": [ { "surname": "Einstein" } ] }"""
    shouldReturnCustomMessage(s, """requirement failed: Author needs one of (organisation | surname and initials) got: {"surname":"Einstein"}""")
  }

  it should "reject an author without surname" in {
    val s: JsonInput = """{ "contributors": [ { "initials": "A" } ] }"""
    shouldReturnCustomMessage(s, """requirement failed: Author needs one of (organisation | surname and initials) got: {"initials":"A"}""")
  }

  it should "reject an organisation with titles" in {
    val s: JsonInput = """{ "contributors": [ { "titles": "A", "organization": "University of Zurich" } ] }"""
    shouldReturnCustomMessage(s, """requirement failed: Author without surname should have neither titles nor insertions; got: {"titles":"A","organization":"University of Zurich"}""")
  }

  it should "reject an organisation with insertions" in {
    val s: JsonInput = """{ "contributors": [ { "insertions": "van der", "organization": "University of Zurich" } ] }"""
    shouldReturnCustomMessage(s, """requirement failed: Author without surname should have neither titles nor insertions; got: {"insertions":"van der","organization":"University of Zurich"}""")
  }

  /** Performs a test that (among others) might break after an upgrade of the json4s library
   *
   * @param s               json string to be deserialized
   * @param expectedMessage messages of exceptions thrown by a require clause of a de-serialized component
   *                        or by custom serializers in JsonUtil
   * @return assertion whether [[JsonUtil.RichJsonInput()]].deserialize.recoverWith
   *         properly un-wrapped the expected custom exception
   */
  private def shouldReturnCustomMessage(s: JsonInput, expectedMessage: String) = {
    inside(DatasetMetadata(s)) { case Failure(InvalidDocumentException(_, t)) =>
      t.getMessage shouldBe expectedMessage
    }
  }
}
