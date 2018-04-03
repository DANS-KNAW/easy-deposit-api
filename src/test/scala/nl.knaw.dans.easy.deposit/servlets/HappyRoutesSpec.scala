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

import java.nio.file.{ Path, Paths }
import java.util.UUID

import nl.knaw.dans.easy.deposit.State._
import nl.knaw.dans.easy.deposit._
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.authentication.AuthenticationProvider
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.DateTime
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Success

class HappyRoutesSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  private class MockedApp extends EasyDepositApiApp(minimalAppConfig)
  private val mockedApp = mock[MockedApp]
  private val userServlet = new UserServlet(mockedApp) {
    override def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider
  }
  private val depositServlet = new DepositServlet(mockedApp) {
    override def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider
  }
  private val authServlet = new AuthServlet(mockedApp) {
    override def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider
  }
  addServlet(depositServlet, "/deposit/*")
  addServlet(userServlet, "/user/*")
  addServlet(authServlet, "/auth/*")
  addServlet(new EasyDepositApiServlet(mockedApp), "/*")

  "get /" should "be ok" in {
    mockedApp.getVersion _ expects() returning "test"
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

  "get /deposit" should "return DepositInfo records" in {
    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    expectsUserFooBar
    (mockedApp.getDeposits(_: String)) expects "foo" returning Success(Seq(
      DepositInfo(uuid1, "x", DRAFT, "a", new DateTime("2018-03-27")),
      DepositInfo(uuid2, "y", SUBMITTED, "b", new DateTime("2018-03-22"))
    ))

    get(
      uri = "/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe OK_200
      val info1 = s"""{"id":"$uuid1","title":"x","state":"DRAFT","stateDescription":"a","timestamp":"2018-03-27"}"""
      val info2 = s"""{"id":"$uuid2","title":"y","state":"SUBMITTED","stateDescription":"b","timestamp":"2018-03-22"}"""
      body shouldBe s"""[$info1,$info2]"""
    }
  }

  "get /user" should "return a user with something for all attributes" ignore {
    expectsUserFooBar
    (mockedAuthenticationProvider.getUser(_: String)) expects "foo" returning Success(Map(
      "uid" -> Seq("foo"),
      "dansPrefixes" -> Seq("van", "den"),
      "sn" -> Seq("Berg"),
      "easyGroups" -> Seq("Archeology", "History")
    ))
    get(
      uri = "/user",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe """{"userName":"foo","firstName":"Jan","prefix":"van de","lastName":"Berg","groups":["Archeology","History"]}"""
      status shouldBe OK_200
    }
  }

  it should "return a user with minimal attributes" ignore {
    expectsUserFooBar
    (mockedAuthenticationProvider.getUser(_: String)) expects "foo" returning Success(Map(
      "uid" -> Seq("foo"),
      "sn" -> Seq("Berg")
    ))

    get(
      uri = "/user",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe """{"userName":"foo","lastName":"Berg"}"""
      status shouldBe OK_200
    }
  }

  private val fixedUUID = UUID.fromString("1cd9409d-8645-46a0-80db-eaf468a5ba7e")

  s"get /$fixedUUID/state" should "return DatasetMetadata" in {
    expectsUserFooBar
    // TODO how to define expects for the curried method called by the PUT variant?
    (mockedApp.getDepositState(_: String, _: UUID)) expects("foo", fixedUUID) returning
      Success(StateInfo(DRAFT, "x"))

    get(
      uri = s"/deposit/$fixedUUID/state",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe s"""{"state":"DRAFT","stateDescription":"x"}"""
      status shouldBe OK_200
    }
  }

  s"get /$fixedUUID/file/a.txt" should "return FileInfo" in {
    expectsUserFooBar
    (mockedApp.getDepositFiles(_: String, _: UUID, _: Path)) expects("foo", fixedUUID, *) returning
      Success(Seq(FileInfo("a.txt", Paths.get("files/a.txt"), "x")))

    get(
      uri = s"/deposit/$fixedUUID/file/a.txt",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe s"""[{"fileName":"a.txt","dirPath":"files/a.txt","sha1sum":"x"}]"""
      status shouldBe OK_200
    }
  }
}
