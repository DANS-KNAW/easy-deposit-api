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
import java.util.{ TimeZone, UUID }

import nl.knaw.dans.easy.deposit._
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.docs.StateInfo.State._
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, DepositInfo, StateInfo }
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.{ DateTime, DateTimeZone }
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Success

class HappyRoutesSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  private class MockedApp extends EasyDepositApiApp(minimalAppConfig)
  private val mockedApp = mock[MockedApp]
  mountServlets(mockedApp, mockedAuthenticationProvider)

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
    (mockedApp.createDeposit(_: String)) expects "foo" returning
      Success(DepositInfo(uuid, title = "just a test"))

    post(
      uri = "/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe CREATED_201
      body shouldBe s"""{"id":"$uuid","title":"just a test","state":"DRAFT","stateDescription":"Deposit is open for changes.","date":"$nowUTC"}"""
      status shouldBe CREATED_201
      body shouldBe s"""{"id":"$uuid","title":"just a test","state":"DRAFT","stateDescription":"Deposit is open for changes.","date":"$nowUTC"}"""
      header("Location") should (fullyMatch regex s"http://localhost:[0-9]+/deposit/$uuid")
    }
  }

  "get /deposit" should "return DepositInfo records" in {
    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    expectsUserFooBar
    (mockedApp.getDeposits(_: String)) expects "foo" returning Success(Seq(
      DepositInfo(uuid1, "x", draft, "a", DateTime.now),
      DepositInfo(uuid2, "y", submitted, "b", DateTime.now)
    ))

    get(
      uri = "/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe OK_200
      // TODO IntegrationSpec seems to apply CEST consistently, should be Z anyway
      val info1 =
        s"""{"id":"$uuid1","title":"x","state":"DRAFT","stateDescription":"a","date":"$nowUTC"}"""
      val info2 = s"""{"id":"$uuid2","title":"y","state":"SUBMITTED","stateDescription":"b","date":"$nowUTC"}"""
      body shouldBe s"""[$info1,$info2]"""
    }
  }

  "get /user" should "return a user with something for all attributes" in {
    expectsUserFooBar
    (mockedApp.getUser(_: String)) expects "foo" returning Success(Map(
      "uid" -> Seq("foo"),
      "cn" -> Seq("Jan"),
      "dansPrefixes" -> Seq("van", "den"),
      "sn" -> Seq("Berg"),
      "easyGroups" -> Seq("Archeology", "History")
    ))
    get(
      uri = "/user",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe """{"userName":"foo","firstName":"Jan","prefix":"van den","lastName":"Berg","groups":["Archeology","History"]}"""
      status shouldBe OK_200
    }
  }

  it should "return a user with minimal attributes" in {
    expectsUserFooBar
    (mockedApp.getUser(_: String)) expects "foo" returning Success(Map(
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

  s"get /deposit/:uuid/state" should "return DatasetMetadata" in {
    expectsUserFooBar
    // TODO how to define expects for the curried method called by the PUT variant?
    (mockedApp.getDepositState(_: String, _: UUID)) expects("foo", uuid) returning
      Success(StateInfo(draft, "x"))

    get(
      uri = s"/deposit/$uuid/state",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe s"""{"state":"DRAFT","stateDescription":"x"}"""
      status shouldBe OK_200
    }
  }

  s"get /deposit/:uuid/file/a.txt" should "return FileInfo" in {
    expectsUserFooBar
    (mockedApp.getDepositFiles(_: String, _: UUID, _: Path)) expects("foo", uuid, *) returning
      Success(Seq(FileInfo("a.txt", Paths.get("files/a.txt"), "x")))

    get(
      uri = s"/deposit/$uuid/file/a.txt",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      body shouldBe s"""[{"fileName":"a.txt","dirPath":"files/a.txt","sha1sum":"x"}]"""
      status shouldBe OK_200
    }
  }

  "put /deposit/:uuid/metadata" should "reject invalid datasetmetadata.json" in {
    expectsUserFooBar

    put(
      uri = s"/deposit/$uuid/metadata",
      body = """{"title":"blabla"}""", // N.B: key should be plural
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe BAD_REQUEST_400
      body shouldBe """Bad Request. invalid DatasetMetadata: class java.lang.Exception don't recognize {"title":"blabla"}"""
    }
  }
}
