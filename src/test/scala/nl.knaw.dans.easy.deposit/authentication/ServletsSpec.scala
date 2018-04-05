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
import nl.knaw.dans.easy.deposit.servlets.ServletFixture
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class ServletsSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  addServlet(new TestServlet(mockedAuthenticationProvider), "/deposit/*")
  addServlet(new AuthTestServlet(mockedAuthenticationProvider), "/auth/*")

  "get /deposit with valid basic authentication" should "be ok" in {
    expectsUserFooBar
    get(
      uri = "/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body should startWith("AuthUser(foo,List(),List(),true) ")
      body should endWith(" EASY Deposit API Service running")
      header("REMOTE_USER") shouldBe "foo"
      header("Set-Cookie") should startWith("scentry.auth.default.user=")
      header("Set-Cookie") shouldNot startWith("scentry.auth.default.user=;") // note the empty value
      status shouldBe OK_200
    }
  }

  "put /auth/logout with valid basic authentication" should "clear and not create a cookie" in {
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
}
