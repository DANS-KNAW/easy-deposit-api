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

import java.nio.file.Paths

import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.JsonUtil._
import org.json4s.JsonInput

import scala.reflect.runtime.universe.typeOf
import scala.util.{ Failure, Success }

class JsonUtilSpec extends TestSupportFixture {

  "enum values" should "be unique across all defined enums" in {
    // see https://github.com/json4s/json4s/issues/142

    val values = enumerations.flatMap(_.values.map(_.toString))
    values shouldBe values.distinct
  }

  "JsonUtil.enumerations" should "should have all enums defined in json objects" in {
    val types = Seq(
      typeOf[DatasetMetadata],
      typeOf[DepositInfo],
      typeOf[StateInfo],
      typeOf[UserInfo],
      typeOf[FileInfo],
    )
    val definedEnumerations = types.flatMap(_.companion.decls
      .filter(_.typeSignature <:< typeOf[Enumeration])
      .map(_.name.toString))

    val registeredEnumerations = enumerations
      .map(_.getClass.getSimpleName.stripSuffix("$"))

    registeredEnumerations should contain allElementsOf definedEnumerations
  }

  "FileInfo" should "serialize with lower case, not camel case" in {
    val fileInfo = FileInfo("test.txt", Paths.get("a/b"), "abc123")
    toJson(fileInfo) shouldBe """{"filename":"test.txt","dirpath":"a/b","sha1sum":"abc123"}"""
  }

  "deserialize" should "reject additional json info" in {
    deserializeAllOptions("""{"x":"foo bar","extra":[1]}""") should matchPattern {
      case Failure(InvalidDocumentException("AllOptions", e))
        if e.getMessage == """don't recognize {"extra":[1]}""" =>
    }
  }

  it should "extract just the last object" in {
    deserializeAllOptions(
      """{"x": "foo bar"}
        |{"xs": [ "123abc" ]
        |}""".stripMargin
    ) should matchPattern {
      case Success(AllOptions(None, Some(_))) =>
    }
  }

  it should "reject an empty array" in {
    deserializeAllOptions("""[]""") should matchPattern {
      case Failure(InvalidDocumentException("AllOptions", e))
        if e.getMessage == """expected an object, got a class org.json4s.JsonAST$JArray""" =>
    }
  }

  it should "reject a literal number" in {
    deserializeAllOptions("""123""") should matchPattern {
      case Failure(InvalidDocumentException("AllOptions", e)) if e.getMessage ==
        """expected field or array
          |Near: 12""".stripMargin =>
    }
  }

  it should "be happy with empty optional objects" in {
    // this is not desired behaviour but documents what actually happens
    deserializeAllOptions("""{}{}""") shouldBe a[Success[_]]
  }

  it should "reject empty input" in {
    deserializeAllOptions(" ") should matchPattern {
      case Failure(InvalidDocumentException("AllOptions", e))
        if e.getMessage == """expected an object, got a class org.json4s.JsonAST$JNothing$""" =>
    }
  }

  it should "reject empty objects when mandatory fields are expected" in {
    deserializeNoOptions("""{}{}""") should matchPattern {
      case Failure(InvalidDocumentException("NoOptions", e)) if e.getMessage ==
        """No usable value for x
          |Did not find value which can be converted into java.lang.String""".stripMargin =>
    }
  }

  private def deserializeAllOptions(input: JsonInput) = {
    input.deserialize[AllOptions]
  }

  private def deserializeNoOptions(input: JsonInput) = {
    input.deserialize[NoOptions]
  }
}
case class AllOptions(x: Option[String], xs: Option[Seq[String]])
case class NoOptions(x: String, xs: Seq[String])
