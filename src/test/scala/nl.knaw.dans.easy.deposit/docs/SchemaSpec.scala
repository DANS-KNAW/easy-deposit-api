package nl.knaw.dans.easy.deposit.docs

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


  "schemas" should "have an up to date UserInfo.json" in {
    verify(factory.createSchema[UserInfo], "UserInfo.json")
  }
  it should "deserialize valid UserInfo" in {
    val validInput = JsonMethods.parse("""{"userName": "john", "lastName": "Doe"}""")
    extract[UserInfo](validInput) should matchPattern { case Right(UserInfo(_, _, _, _, _)) => }
  }
  it should "report invalid UserInfo" in {
    val validInput = JsonMethods.parse("""{"userName": "john", "foo": "bar"}""")
    inside(extract[UserInfo](validInput)) {
      case Left(List(
      ValidationError(path1, JNothing, MissingProperty(_)),
      ValidationError(path2, JString(val2), UnexpectedProperty(_)),
      )) =>
        path1 shouldBe "lastName"
        path2 shouldBe "foo"
        val2 shouldBe "bar"
    }
  }

  it should "have an up to date StateInfo.json" ignore {
    // package object causes java.lang.ClassNotFoundException: nl.knaw.dans.easy.deposit.StateInfo
    // at fi.oph.scalaschema.SchemaFactory.typeByName(SchemaFactory.scala:48)
    // TODO workaround: move StateInfo out of package object to docs package, is more consistent anyway
    verify(factory.createSchema[StateInfo], "StateInfo.json")
  }
  it should "have an up to date DepositInfo.json" ignore {
    // enum causes java.lang.ClassNotFoundException: scala.Enumeration.Value
    // at fi.oph.scalaschema.TraitImplementationFinder$.$anonfun$findTraitImplementations$1(SchemaFactory.scala:302)
    verify(factory.createSchema[DepositInfo], "DepositInfo.json")
  }

  it should "have an up to date EnumPoc.json" in {
    verify(factory.createSchema[EnumPoc], "EnumPoc.json")
  }
  it should "report invalid Enum value" in {
    val validInput = JsonMethods.parse("""{"privacySensitiveDataPresent2": "foo"}""")
    inside(extract[EnumPoc](validInput)) {
      case Left(List(ValidationError(path1, JString(val1), NotAnyOf(m, _)))) =>
        path1 shouldBe "privacySensitiveDataPresent2"
        val1 shouldBe "foo"
        m shouldBe Map("no" -> List("object expected"), "unspecified" -> List("object expected"), "yes" -> List("object expected"))
    }
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


