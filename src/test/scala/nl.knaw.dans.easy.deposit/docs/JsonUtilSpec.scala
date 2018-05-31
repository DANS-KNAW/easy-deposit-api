package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.TestSupportFixture

class JsonUtilSpec extends TestSupportFixture {

  "enum values" should "be unique across all defined enums" in {
    // see https://github.com/json4s/json4s/issues/142

    val values = JsonUtil.enumerations.flatMap(_.values.map(_.toString))
    values shouldBe values.distinct
  }
}
