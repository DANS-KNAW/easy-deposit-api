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
package nl.knaw.dans.easy.deposit.servlets

import java.util.UUID

import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.authentication.{ AuthUser, AuthenticationProvider }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata
import nl.knaw.dans.easy.deposit._
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.auth.Scentry
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Failure

class DepositServletErrorSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  private class MockedApp extends EasyDepositApiApp(minimalAppConfig)
  private val mockedApp = mock[MockedApp]
  private val depositServlet = new DepositServlet(mockedApp) {
    override def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider
  }
  addServlet(depositServlet, "/*")

  "post /" should "return 500 (Internal Server Error) on a not expected exception and basic authentication" in {
    (mockedApp.createDeposit(_: String)) expects "foo" returning Failure(new Exception("whoops"))
    expectsUserFooBar
    post(uri = "/", headers = Seq(("Authorization", fooBarBasicAuthHeader))) {
      status shouldBe INTERNAL_SERVER_ERROR_500
      body shouldBe "Internal Server Error"
    }
  }

  it should "return 500 (Internal Server Error) on a not expected exception and a cookie" ignore {
    // TODO breaks tests in TypicalSessionSpec when running 'mvn clean install'
    val jwtCookie = createJWT(AuthUser("foo", isActive = true))
    expectsUserFooBar
    post(uri = "/", headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=$jwtCookie"))) {
      status shouldBe INTERNAL_SERVER_ERROR_500
      body shouldBe "Internal Server Error"
    }
  }

  it should "not return an uncatched exception" in {
    // the test for not expected exceptions show the authentication method doesn't matter
    (mockedApp.createDeposit(_: String)) expects "foo" throwing new Exception("trouble in paradise")
    expectsUserFooBar
    post(uri = "/", headers = Seq(("Authorization", fooBarBasicAuthHeader))) {
      status shouldBe INTERNAL_SERVER_ERROR_500
      body shouldBe "Internal Server Error"
    }
  }

  s"put /:uuid/metadata" should "report a lost dataset" in {
    expectsUserFooBar
    (mockedApp.writeDataMetadataToDeposit(_: DatasetMetadata)(_: String, _: UUID)) expects(*, "foo", uuid) returning
      Failure(NoSuchDepositException("foo", uuid, new Exception()))

    put(
      uri = s"/$uuid/metadata",
      body = """{"blabla":"blabla"}""",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe NOT_FOUND_404
      body shouldBe s"Deposit $uuid not found"
    }
  }

  s"get /:uuid/metadata" should "report a currupt dataset" in {
    (mockedApp.getDatasetMetadataForDeposit(_: String, _: UUID)) expects("foo", uuid) returning
      Failure(CorruptDepositException("foo", uuid.toString, new Exception("invalid json")))

    expectsUserFooBar
    get(
      uri = s"/$uuid/metadata",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe s"Internal Server Error"
      status shouldBe INTERNAL_SERVER_ERROR_500
    }
  }

  it should "report a missing dataset" in {
    (mockedApp.getDatasetMetadataForDeposit(_: String, _: UUID)) expects("foo", uuid) returning
      Failure(NoSuchDepositException("foo", uuid, new Exception("file not found")))

    expectsUserFooBar
    get(
      uri = s"/$uuid/metadata",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe s"Deposit $uuid not found"
      status shouldBe NOT_FOUND_404
    }
  }
}
