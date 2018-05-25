package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.TestSupportFixture

class JsonSpec extends TestSupportFixture {

  "enum values" should "be unique" in {
    // see https://github.com/json4s/json4s/issues/142

    val values = Json.enumerations.flatMap(_.values.map(_.toString))
    values shouldBe values.distinct
  }
}