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
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.format.DateTimeFormat
import org.scalamock.scalatest.MockFactory
import org.scalatra.auth.Scentry
import org.scalatra.test.scalatest.ScalatraSuite

class TypicalSessionSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  addServlet(new TestServlet(mockedAuthenticationProvider), "/deposit/*")
  addServlet(new AuthTestServlet(mockedAuthenticationProvider), "/auth/*")

  private class TokenSupportImpl() extends TokenSupport with AuthConfig {

    def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider

    def getProperties: PropertiesConfiguration = new PropertiesConfiguration()
  }
  private val tokenSupport = new TokenSupportImpl()



  "get /deposit without credentials" should "return 403 (forbidden)" in {
    expectsNoUser
    get("/deposit") {
      status shouldBe FORBIDDEN_403
      body shouldBe "missing, invalid or expired credentials"
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      Option(header("Set-Cookie")) shouldBe None
    }
  }

  "post /auth/login with invalid credentials" should "return 403 (forbidden)" in {
    expectsInvalidUser
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      body shouldBe "invalid credentials"
      status shouldBe FORBIDDEN_403
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      Option(header("Set-Cookie")) shouldBe None
    }
  }

  "post /auth/login with proper user-name password" should "create a protected cookie" in {
    expectsUserFooBar
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      status shouldBe OK_200
      body shouldBe "signed in"
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      header("REMOTE_USER") shouldBe "foo"
      val newCookie = header("Set-Cookie")
      newCookie should startWith("scentry.auth.default.user=")
      newCookie should include(";Path=/")
      newCookie should include(";HttpOnly")
      cookieAge(newCookie) should be < 1000L
    }
  }

  "post /auth/login with valid basic authentication" should "create a cookie" in {
    expectsUserFooBar
    post(
      uri = "/auth/login",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe "signed in"
      status shouldBe OK_200
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      header("REMOTE_USER") shouldBe "foo"
      val newCookie = header("Set-Cookie")
      newCookie should startWith("scentry.auth.default.user=")
      newCookie should include(";Path=/")
      newCookie should include(";HttpOnly")
      cookieAge(newCookie) should be < 1000L
    }
  }

  "get /deposit with valid cookie token" should "be ok" in {
    expectsNoUser
    val jwtCookie = tokenSupport.encodeJWT(AuthUser("foo", isActive = true))

    get(
      uri = "/deposit",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))
    ) {
      status shouldBe OK_200
      Option(header("REMOTE_USER")) shouldBe None
      body should startWith("AuthUser(foo,List(),List(),true) ")
      body should endWith(" EASY Deposit API Service running")
    }
  }

  "put /auth/logout" should "clear the cookie" in {
    expectsNoUser
    val jwtCookie = tokenSupport.encodeJWT(AuthUser("foo", isActive = true))

    put(
      uri = "/auth/logout",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))
    ) {
      status shouldBe OK_200
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      header("REMOTE_USER") shouldBe ""
      val newCookie = header("Set-Cookie")
      newCookie should startWith("scentry.auth.default.user=;") // note the empty value
      newCookie should include(";Path=/")
      newCookie should include(";Expires=")
      newCookie should include(";HttpOnly")
    }
  }

  def cookieAge(cookie: String): Long = {
    val expiresString = cookie
      .replaceAll(".*Expires=", "")
      .replaceAll(";.*", "")
    val expiresLong = DateTimeFormat
      .forPattern("EEE, dd-MMM-yyyy HH:mm:ss zzz")
      .parseDateTime(expiresString)
      .getMillis
    expiresLong -
      (tokenSupport.tokenConfig.expiresIn * 1000) -
      System.currentTimeMillis
  }
}
