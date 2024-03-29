/*
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
import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState
import nl.knaw.dans.easy.deposit.servlets.{ AuthServlet, ProtectedServlet, ServletFixture, UndoMasking }
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatra.Ok
import org.scalatra.auth.Scentry
import org.scalatra.test.scalatest.ScalatraSuite

class SessionSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  private val app: EasyDepositApiApp = createTestApp()
  private val authMocker = new AuthenticationMocker() {
    override val mockedAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]
  }

  private val authServlet = new AuthServlet(app) with UndoMasking {
    override def getAuthenticationProvider: AuthenticationProvider = authMocker.mockedAuthenticationProvider
  }
  private val testServlet = new ProtectedServlet(app) with UndoMasking {
    override def getAuthenticationProvider: AuthenticationProvider = authMocker.mockedAuthenticationProvider

    get("/") {
      contentType = "text/plain"
      Ok(s"$user ${ new DateTime() }: EASY Deposit API Service running")
    }
  }

  addServlet(testServlet, "/deposit/*")
  addServlet(authServlet, "/auth/*")

  "GET /deposit" should "return 401 (Unauthorized) when neither cookie nor basic authentication are provided" in {
    authMocker.expectsNoUser
    get("/deposit") {
      status shouldBe UNAUTHORIZED_401
      body shouldBe "missing, invalid or expired credentials"
      response.header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  it should "be ok when logging in with a valid user-name password" in {
    authMocker.expectsUserFooBar
    get(
      uri = "/deposit",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body should startWith("AuthUser(foo,ACTIVE) ")
      body should endWith(" EASY Deposit API Service running")
      response.header("REMOTE_USER") shouldBe "foo"
      response.header("Set-Cookie") should startWith regex "scentry.auth.default.user=[^;].+;"
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
      response.header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
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
      response.header("Content-Type").toLowerCase shouldBe "text/html;charset=utf-8"
      response.header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      response.header("REMOTE_USER") shouldBe ""
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
      response.header("REMOTE_USER") shouldBe ""
      status shouldBe NO_CONTENT_204
      shouldHaveEmptyCookie(2)
    }
  }

  "POST /auth/login" should "create a cookie when valid basic authentication is provided" in {
    authMocker.expectsUserFooBarWithStatus(UserState.active)
    post(
      uri = "/auth/login",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe ""
      status shouldBe NO_CONTENT_204
      response.header("Expires") shouldBe "Thu, 01 Jan 1970 00:00:00 GMT" // page cache
      response.header("REMOTE_USER") shouldBe "foo"
      val newCookie = response.header("Set-Cookie")
      newCookie should startWith regex "scentry.auth.default.user=[^;].+;"
      newCookie should include regex """;\s*Path\s*="""
      newCookie should include regex """;\s*HttpOnly"""
      cookieAge(newCookie) should be < 1000L
    }
  }

  it should "return 401 (Unauthorized) when invalid user-name password params are provided" in {
    authMocker.expectsInvalidUser
    post(
      uri = "/auth/login",
      params = Seq(("login", "foo"), ("password", "bar"))
    ) {
      body shouldBe "invalid credentials"
      status shouldBe UNAUTHORIZED_401
      response.header("Content-Type").toLowerCase shouldBe "text/plain;charset=utf-8"
      response.headers should not contain key("Set-Cookie")
    }
  }

  private def shouldHaveEmptyCookie(nrOfCookies: Int): Any = {
    val newCookie = response.headers("Set-Cookie")
    val lastCookie = newCookie.lastOption.getOrElse("")
    lastCookie should include("scentry.auth.default.user=;") // note the empty value
    lastCookie should include regex """;\s*Path\s*=\s*/"""
    lastCookie should include regex """;\s*Expires\s*="""
    lastCookie should include regex """;\s*HttpOnly"""
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
