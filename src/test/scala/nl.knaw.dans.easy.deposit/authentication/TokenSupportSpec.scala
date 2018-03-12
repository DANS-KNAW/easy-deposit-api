package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.TestSupportFixture

import scala.util.Success

class TokenSupportSpec extends TestSupportFixture with TokenSupport {
  mockDateTimeNow("2018-03-12")

  "toUser" should "return encoded value" in {
    val user = AuthUser("foo", isActive = true)
    toUser(encode(user)) shouldBe Success(user)
  }
}
