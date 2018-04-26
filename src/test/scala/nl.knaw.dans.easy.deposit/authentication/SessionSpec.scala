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
import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState.ACTIVE
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.servlets.ServletFixture
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.format.DateTimeFormat
import org.scalamock.scalatest.MockFactory
import org.scalatra.auth.Scentry
import org.scalatra.test.scalatest.ScalatraSuite

class SessionSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  addServlet(new TestServlet(mockedAuthenticationProvider), "/deposit/*")
  addServlet(new AuthTestServlet(mockedAuthenticationProvider), "/auth/*")

  "GET /deposit" should "return 401 (Unauthorized) when neither cookie nor login params are provided" in {
    expectsNoUser
    get("/deposit") {
      status shouldBe UNAUTHORIZED_401
      body shouldBe "missing, invalid or expired credentials"
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  it should "be ok when logging in on the flight with valid basic authentication" in {
    expectsUserFooBar
    get(
      uri = "/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body should startWith("AuthUser(foo,List(),ACTIVE) ")
      body should endWith(" EASY Deposit API Service running")
      header("REMOTE_USER") shouldBe "foo"
      header("Set-Cookie") should startWith("scentry.auth.default.user=")
      header("Set-Cookie") shouldNot startWith("scentry.auth.default.user=;") // note the empty value
      status shouldBe OK_200
    }
  }

  it should "be ok with valid cookie token" in {
    expectsNoUser
    val jwtCookie = createJWT(AuthUser("foo", state = ACTIVE))

    get(
      uri = "/deposit",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))
    ) {
      status shouldBe OK_200
      Option(header("REMOTE_USER")) shouldBe None
      body should startWith("AuthUser(foo,List(),ACTIVE) ")
      body should endWith(" EASY Deposit API Service running")
    }
  }

  it should "fail with invalid cookie token" in {
    expectsNoUser
    val jwtCookie = "invalid cookie"

    get(
      uri = "/deposit",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))
    ) {
      status shouldBe UNAUTHORIZED_401
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  "POST /auth/login" should "return 401 (Unauthorized) when invalid user-name password params are provided" in {
    expectsInvalidUser
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      body shouldBe "invalid credentials"
      status shouldBe UNAUTHORIZED_401
      header("Content-Type") shouldBe "text/plain;charset=UTF-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  it should "create a protected cookie when proper user-name password params are provided" in {
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

  it should "create a cookie when valid basic authentication is provided" in {
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

  "put /auth/logout" should "clear the cookie" in {
    expectsNoUser
    val jwtCookie = createJWT(AuthUser("foo", state = ACTIVE))

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

  it should "not create a cookie if called with basic authentication" in {
    expectsUserFooBar
    put(
      uri = "/auth/logout",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe "you are signed out"
      header("REMOTE_USER") shouldBe ""
      header("Set-Cookie") should startWith("scentry.auth.default.user=;")
      status shouldBe OK_200
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
      (jwtConfig.expiresIn * 1000) -
      System.currentTimeMillis
  }
}
