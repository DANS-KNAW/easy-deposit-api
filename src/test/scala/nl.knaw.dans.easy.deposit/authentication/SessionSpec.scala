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

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit._
import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState
import nl.knaw.dans.easy.deposit.logging._
import nl.knaw.dans.easy.deposit.servlets.{ AuthServlet, ProtectedServlet, ServletFixture }
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatra.Ok
import org.scalatra.auth.Scentry
import org.scalatra.test.scalatest.ScalatraSuite

class SessionSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  private val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
    override val pidRequester: PidRequester = mock[PidRequester]
  }
  private val authMocker = new AuthenticationMocker() {
    override val mockedAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]
  }
  private val authServlet = new AuthServlet(app) with PlainHeaders with PlainCookies with PlainRemoteAddress {
    override def getAuthenticationProvider: AuthenticationProvider = authMocker.mockedAuthenticationProvider
  }
  private val testServlet = new ProtectedServlet(app) with PlainHeaders with PlainCookies {
    override def getAuthenticationProvider: AuthenticationProvider = authMocker.mockedAuthenticationProvider

    get("/") {
      contentType = "text/plain"
      Ok(s"$user ${ new DateTime() }: EASY Deposit API Service running")
        .logResponse
    }
  }

  addServlet(testServlet, "/deposit/*")
  addServlet(authServlet, "/auth/*")

  "GET /deposit" should "return 401 (Unauthorized) when neither cookie nor login params are provided" in {
    authMocker.expectsNoUser
    get("/deposit") {
      status shouldBe UNAUTHORIZED_401
      body shouldBe "missing, invalid or expired credentials"
      header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  it should "be ok when logging in on the flight with valid basic authentication" in {
    authMocker.expectsUserFooBar
    get(
      uri = "/deposit",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body should startWith("AuthUser(foo,ACTIVE) ")
      body should endWith(" EASY Deposit API Service running")
      header("REMOTE_USER") shouldBe "foo"
      header("Set-Cookie") should startWith regex "scentry.auth.default.user=[^;].+;"
      status shouldBe OK_200
    }
  }

  it should "be ok with valid cookie token" in {
    authMocker.expectsNoUser
    val jwtCookie = createJWT(AuthUser("foo", state = UserState.active))

    get(
      uri = "/deposit",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))
    ) {
      status shouldBe OK_200
      Option(header("REMOTE_USER")) shouldBe None
      body should startWith("AuthUser(foo,ACTIVE) ")
      body should endWith(" EASY Deposit API Service running")
    }
  }

  it should "fail with invalid cookie token" in {
    authMocker.expectsNoUser
    val jwtCookie = "invalid cookie"

    get(
      uri = "/deposit",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))
    ) {
      status shouldBe UNAUTHORIZED_401
      header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  "POST /auth/login" should "return 401 (Unauthorized) when invalid user-name password params are provided" in {
    authMocker.expectsInvalidUser
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      body shouldBe "invalid credentials"
      status shouldBe UNAUTHORIZED_401
      header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  it should "return 401 (Unauthorized) when the user has not confirmed the email" in {
    authMocker.expectsUserFooBarWithStatus(UserState.registered)
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      body shouldBe "Please confirm your email."
      status shouldBe UNAUTHORIZED_401
      header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  it should "return 401 (Unauthorized) when the user is blocked" in {
    authMocker.expectsUserFooBarWithStatus(UserState.blocked)
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      body shouldBe "invalid credentials"
      status shouldBe UNAUTHORIZED_401
      header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  it should "create a protected cookie when proper user-name password params are provided" in {
    authMocker.expectsUserFooBar
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      status shouldBe NO_CONTENT_204
      body shouldBe ""
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      header("REMOTE_USER") shouldBe "foo"
      val newCookie = header("Set-Cookie")
      newCookie should startWith regex "scentry.auth.default.user=[^;].+;"
      newCookie should include(";Path=/")
      newCookie should include(";HttpOnly")
      cookieAge(newCookie) should be < 1000L
    }
  }

  it should "create a cookie when valid basic authentication is provided" in {
    authMocker.expectsUserFooBarWithStatus(UserState.active)
    post(
      uri = "/auth/login",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      header("REMOTE_USER") shouldBe "foo"
      val newCookie = header("Set-Cookie")
      newCookie should startWith regex "scentry.auth.default.user=[^;].+;"
      newCookie should include(";Path=/")
      newCookie should include(";HttpOnly")
      cookieAge(newCookie) should be < 1000L
    }
  }

  "post /auth/logout" should "clear the cookie" in {
    authMocker.expectsNoUser
    val jwtCookie = createJWT(AuthUser("foo", state = UserState.active))

    post(
      uri = "/auth/logout",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))
    ) {
      status shouldBe NO_CONTENT_204
      header("Content-Type").toLowerCase shouldBe "text/html;charset=utf-8"
      header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      header("REMOTE_USER") shouldBe ""
      shouldHaveEmptyCookie(1)
    }
  }

  it should "not create a cookie if called with basic authentication" in {
    authMocker.expectsUserFooBar
    post(
      uri = "/auth/logout",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe ""
      header("REMOTE_USER") shouldBe ""
      status shouldBe NO_CONTENT_204
      // TODO basic authentication is intended for internal test purposes
      //  if exposed beyond the firewall we need to prevent the first cookie
      shouldHaveEmptyCookie(2)
    }
  }

  private def shouldHaveEmptyCookie(nrOfCookies: Int): Any = {
    val newCookie = response.headers("Set-Cookie")
    val lastCookie = newCookie.lastOption.getOrElse("")
    lastCookie should include("scentry.auth.default.user=;") // note the empty value
    lastCookie should include(";Path=/")
    lastCookie should include(";Expires=")
    lastCookie should include(";HttpOnly")
    newCookie.size shouldBe nrOfCookies
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
