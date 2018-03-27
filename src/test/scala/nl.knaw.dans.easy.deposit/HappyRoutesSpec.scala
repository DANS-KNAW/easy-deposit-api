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
package nl.knaw.dans.easy.deposit

import java.util.UUID

import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.authentication.AuthenticationProvider
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Success

class HappyRoutesSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  private class MockedApp extends EasyDepositApiApp(minimalAppConfig)
  private val mockedApp = mock[MockedApp]
  private val depositServlet: DepositServlet = new DepositServlet(mockedApp) {
    override def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider
  }
  private val authServlet = new AuthServlet(mockedApp) {
    override def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider
  }
  addServlet(depositServlet, "/deposit/*")
  addServlet(authServlet, "/auth/*")
  addServlet(new EasyDepositApiServlet(mockedApp), "/*")

  "get /" should "be ok" in {
    mockedApp.getVersion _ expects () returning "test"
    expectsNoUser
    get(uri = "/") {
      body shouldBe "EASY Deposit API Service running (test)"
      status shouldBe OK_200
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
      header("Set-Cookie") should startWith("scentry.auth.default.user=") // details in TypicalSessionSpec
    }
  }

  "post /deposit" should "create a deposit" in {
    val uuid = UUID.randomUUID()
    expectsUserFooBar
    (mockedApp.createDeposit(_: String)) expects "foo" returning Success(uuid)

    post(
      uri = "/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe OK_200
      body shouldBe s"$uuid"
      header("Location") should (fullyMatch regex s"http://localhost:[0-9]+/deposit/$uuid")
    }
  }
}
