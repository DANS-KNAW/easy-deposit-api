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

import java.util.UUID

import better.files.File
import fi.oph.scalaschema.SchemaValidatingExtractor.extract
import fi.oph.scalaschema.extraction._
import fi.oph.scalaschema.{ ExtractionContext, Schema, SchemaFactory }
import nl.knaw.dans.easy.deposit.{ StateInfo, TestSupportFixture }
import org.json4s.JsonAST.{ JNothing, JString }
import org.json4s.jackson.JsonMethods
import org.scalatest.Assertion

class SchemaSpec extends TestSupportFixture {

  private val factory = SchemaFactory.default
  private val testSchemas = (testDir / "schemas").createIfNotExists(asDirectory = true, createParents = true)
  private val docsSchemas = File("docs") / "schemas"
  private implicit val context: ExtractionContext = ExtractionContext(SchemaFactory.default)

  // TODO needs: https://github.com/rvanheest/scala-schema/tree/remove-forName-calls

  "UserInfo" should "have an up to date schema file" in {
    verify(factory.createSchema[UserInfo], "UserInfo.json")
  }

  it should "deserialize valid json input" in {
    extract[UserInfo]("""{"userName": "john", "lastName": "Doe"}""") should
      matchPattern { case Right(UserInfo(_, _, _, _, _)) => }
  }

  it should "report invalid json input" in {
    inside(extract[UserInfo]("""{"userName": "john", "foo": "bar"}""")) {
      case Left(List(
      ValidationError(path1, JNothing, MissingProperty(_)),
      ValidationError(path2, JString(val2), UnexpectedProperty(_)),
      )) =>
        path1 shouldBe "lastName"
        path2 shouldBe "foo"
        val2 shouldBe "bar"
    }
  }

  "StateInfo" should "have an up to date schema file" in {
    verify(factory.createSchema[StateInfo], "StateInfo.json")
  }

  it should "deserialize valid json input" ignore { // TODO DRAFT causes NotAnyOf

    // compare with error message for EnumPoc
    NotAnyOf(Map(
      "mutablesettings$multichoiceenumeration$choice" -> ???,
      "warnings$lintwarnings$lintwarning" -> ???,
      "enumeration$val" -> ???,
      "frontend$severity" -> ???
    ))

    extract[StateInfo]("""{"state": "DRAFT", "stateDescription": "rabarbera"}""") should
      matchPattern { case Right(StateInfo(_, _)) => }
  }

  "DatasetMetadata" should "have an up to date schema file" in {
    verify(factory.createSchema[StateInfo], "DatasetMetadata.json")
  }

  "DepositInfo" should "have an up to date schema file" in {
    verify(factory.createSchema[DepositInfo], "DepositInfo.json")
  }

  it should "deserialize valid json input" ignore {
    val uuid = UUID.randomUUID() // TODO causes UnexpectedType
    extract[DepositInfo](s"""{"id": "$uuid"}""") should
      matchPattern { case Right(DepositInfo(_, _, _, _, _)) => }
  }

  "EnumPoc" should "have an up to date schema file" in {
    verify(factory.createSchema[EnumPoc], "EnumPoc.json")
  }

  it should "report invalid json input" in {
    val result = extract[EnumPoc]("""{"privacySensitiveDataPresent2": "foo"}""")
    // TODO make readable for end user
    result.left.getOrElse("").toString shouldBe
      "List(ValidationError(privacySensitiveDataPresent2,JString(foo),NotAnyOf(Map(no -> List(object expected), unspecified -> List(object expected), yes -> List(object expected)),notAnyOf)))"
  }

  private def verify(schemaAsJson: Schema, file: String): Assertion = {
    val schemaAsString = JsonMethods.pretty(schemaAsJson.toJson)
    (testSchemas / file).writeText(schemaAsString)
    (docsSchemas / file).contentAsString shouldBe schemaAsString
  }
}

// TODO apply this enum proof of concept to all objects in docs package that extend Enumeration
case class EnumPoc(doi: Option[String] = None,
                   privacySensitiveDataPresent2: PrivacySensitiveDataPresent2 = UNSPECIFIED,
                  )

//////////////////// alternative for: object Xyz extends Enumeration
// TODO move into own package

sealed abstract class PrivacySensitiveDataPresent2(val name: String) {
  // see also  https://github.com/Opetushallitus/scala-schema/issues/2
  // and the example https://github.com/Opetushallitus/scala-schema/blob/0f1ab960d71fe16c1af4e0ebb1686ce0b4811c48/src/test/scala/fi/oph/scalaschema/TestData.scala#L98-L102
  override def toString: String = name
}
object PrivacySensitiveDataPresent2 {
  def parse(name: String): Option[PrivacySensitiveDataPresent2] = {
    name match {
      case "yes" => Some(YES)
      case "no" => Some(NO)
      case "unspecified" => Some(UNSPECIFIED)
      case _ => None
    }
  }
}
case object YES extends PrivacySensitiveDataPresent2("yes")
case object NO extends PrivacySensitiveDataPresent2("no")
case object UNSPECIFIED extends PrivacySensitiveDataPresent2("unspecified")


