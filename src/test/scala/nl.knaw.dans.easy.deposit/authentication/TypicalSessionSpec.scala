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
package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit._
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.format.DateTimeFormat
import org.scalatra.auth.Scentry
import org.scalatra.test.scalatest.ScalatraSuite

class TypicalSessionSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  val depositApp: EasyDepositApiApp = new EasyDepositApiApp(new Configuration("test", new PropertiesConfiguration() {
    addProperty("users.ldap-url", "ldap://hostDoesNotExist")
    addProperty("deposits.drafts", s"$testDir/drafts")
  })) {
    override val authentication: Authentication = mock[Authentication]
  }

  private def receivedToken = new TokenSupport() {
    override def getTokenConfig: TokenConfig = TokenConfig()
  }.encode(AuthUser("foo", isActive = true))

  addServlet(new EasyDepositApiServlet(depositApp), "/deposit/*")
  addServlet(new AuthenticationServlet(depositApp), "/auth/*")

  "get /deposit without credentials" should "redirect to /auth/signin" in {
    (depositApp.authentication.getUser(_: String, _: String)) expects(*, *) never()
    get("/deposit") {
      status shouldBe FORBIDDEN_403
      body shouldBe "missing, invalid or expired credentials"
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      Option(header("Set-Cookie")) shouldBe None
    }
  }

  "get /auth/signin" should "present a login form" in {
    (depositApp.authentication.getUser(_: String, _: String)) expects(*, *) never()
    get("/auth/signin") {
      status shouldBe OK_200
      body should include("""<label for="login">""")
      Option(header("Set-Cookie")) shouldBe None
      header("Content-Type") shouldBe "text/html;charset=UTF-8"
    }
  }

  "post /auth with invalid credentials" should "redirect to /auth/signin (again)" in {
    (depositApp.authentication.getUser(_: String, _: String)) expects("foo", "bar") returning None
    post(
      uri = "/auth",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      status shouldBe FORBIDDEN_403
      body shouldBe "invalid credentials"
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      Option(header("Set-Cookie")) shouldBe None
    }
  }

  "post /auth with proper user-name password" should "create a protected cookie" in {
    (depositApp.authentication.getUser(_: String, _: String)) expects("foo", "bar") returning
      Some(AuthUser("foo", isActive = true))
    post(
      uri = "/auth",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      status shouldBe OK_200
      body shouldBe "signed in"
      header("REMOTE_USER") shouldBe "foo"
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      val newCookie = header("Set-Cookie")
      newCookie should startWith("scentry.auth.default.user=")
      newCookie should include(";Path=/")
      newCookie should include(";HttpOnly")

      // check cookie expiration
      val expiresString = newCookie
        .replaceAll(".*Expires=", "")
        .replaceAll(";.*", "")
      val expiresLong = DateTimeFormat
        .forPattern("EEE, dd-MMM-yyyy HH:mm:ss zzz")
        .parseDateTime(expiresString)
        .getMillis
      val cookieAge = expiresLong -
        (depositApp.authCookieOptions.maxAge * 1000) -
        System.currentTimeMillis
      cookieAge should be < 1000L
    }
  }

  "get /deposit with valid cookie token" should "be ok" in {

    (depositApp.authentication.getUser(_: String, _: String)) expects(*, *) never()

    get(
      uri = "/deposit",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$receivedToken"))
    ) {
      status shouldBe OK_200
      body should startWith("AuthUser(foo,List(),List(),true) ")
      body should endWith(" EASY Deposit API Service running (test)")
    }
  }

  "get /auth/signout" should "clear the cookie" in {
    (depositApp.authentication.getUser(_: String, _: String)) expects(*, *) never()

    get(
      uri = "/auth/signout",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$receivedToken"))
    ) {
      status shouldBe OK_200
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      val newCookie = header("Set-Cookie")
      newCookie should startWith("scentry.auth.default.user=;") // note the empty value
      newCookie should include(";Path=/")
      newCookie should include(";Expires=")
      newCookie should include(";HttpOnly")
    }
  }
}
