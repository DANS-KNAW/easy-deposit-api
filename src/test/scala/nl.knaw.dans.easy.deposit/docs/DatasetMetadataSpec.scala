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
import org.json4s.Diff
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

import scala.util.{ Failure, Success }

class DatasetMetadataSpec extends TestSupportFixture {

  "deserialization/serialisation" should "produce the same json object structure" in {
    val example = (File("src") / "test" / "resources" / "manual-test" / "datasetmetadata.json").contentAsString
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
        |      "scheme": "id-type:doi",
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
}
