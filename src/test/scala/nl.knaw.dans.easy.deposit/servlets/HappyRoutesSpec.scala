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

import nl.knaw.dans.easy.deposit._
import nl.knaw.dans.easy.deposit.authentication.{ AuthenticationMocker, AuthenticationProvider }
import nl.knaw.dans.easy.deposit.docs.StateInfo.State._
import nl.knaw.dans.easy.deposit.docs._
import org.eclipse.jetty.http.HttpStatus._
import org.joda.time.DateTime
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.Success

class HappyRoutesSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  private val authMocker = new AuthenticationMocker() {
    override val mockedAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]
  }
  private class MockedApp extends EasyDepositApiApp(minimalAppConfig)
  private val mockedApp = mock[MockedApp]
  mountServlets(mockedApp, authMocker.mockedAuthenticationProvider)

  "get /" should "be ok" in {
    (() => mockedApp.getVersion) expects() returning "test"
    authMocker.expectsNoUser
    get(uri = "/") {
      body shouldBe "EASY Deposit API Service running (test)"
      status shouldBe OK_200
    }
  }

  "post /deposit" should "create a deposit" in {
    val uuid = UUID.randomUUID()
    authMocker.expectsUserFooBar
    (mockedApp.createDeposit(_: String)) expects "foo" returning
      Success(DepositInfo(uuid, title = "just a test"))

    post(
      uri = "/deposit",
      headers = Seq(fooBarBasicAuthHeader)
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
    authMocker.expectsUserFooBar
    (mockedApp.getDeposits(_: String)) expects "foo" returning Success(Seq(
      DepositInfo(uuid1, "x", draft, "a", DateTime.now),
      DepositInfo(uuid2, "y", submitted, "b", DateTime.now)
    ))

    get(
      uri = "/deposit",
      headers = Seq(fooBarBasicAuthHeader)
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
    authMocker.expectsUserFooBar
    (mockedApp.getUserProperties(_: String)) expects "foo" returning
      Success(UserInfo("foo", displayName = "Jan v.d. Berg", email ="", firstName = Some("Jan"), prefix = Some("van den"), lastName="Berg"))
    get(
      uri = "/user",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe """{"userName":"foo","firstName":"Jan","prefix":"van den","lastName":"Berg","displayName":"Jan v.d. Berg","email":""}"""
      status shouldBe OK_200
    }
  }

  it should "return a user with minimal attributes" in {
    authMocker.expectsUserFooBar
    (mockedApp.getUserProperties(_: String)) expects "foo" returning
      Success(UserInfo("foo", displayName = "", email ="", lastName=""))

    get(
      uri = "/user",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe """{"userName":"foo","lastName":"","displayName":"","email":""}"""
      status shouldBe OK_200
    }
  }

  s"get /deposit/:uuid/state" should "return DatasetMetadata" in {
    authMocker.expectsUserFooBar
    // TODO how to define expects for the curried method called by the PUT variant?
    (mockedApp.getDepositState(_: String, _: UUID)) expects("foo", uuid) returning
      Success(StateInfo(draft, "x"))

    get(
      uri = s"/deposit/$uuid/state",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe s"""{"state":"DRAFT","stateDescription":"x"}"""
      status shouldBe OK_200
    }
  }

  s"get /deposit/:uuid/file/path/to/directory" should "return a list with one FileInfo object in json format" in {
    authMocker.expectsUserFooBar
    (mockedApp.getFileInfo(_: String, _: UUID, _: Path)) expects("foo", uuid, *) returning
      Success(Seq(FileInfo("a.txt", Paths.get("files"), "x")))

    get(
      uri = s"/deposit/$uuid/file/path/to/directory",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe s"""[{"filename":"a.txt","dirpath":"files","sha1sum":"x"}]"""
      status shouldBe OK_200
    }
  }

  s"get /deposit/:uuid/file/a.txt" should "return a FileInfo object in json format" in {
    authMocker.expectsUserFooBar
    (mockedApp.getFileInfo(_: String, _: UUID, _: Path)) expects("foo", uuid, *) returning
      Success(FileInfo("a.txt", Paths.get("files"), "x"))

    get(
      uri = s"/deposit/$uuid/file/a.txt",
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      body shouldBe s"""{"filename":"a.txt","dirpath":"files","sha1sum":"x"}"""
      status shouldBe OK_200
    }
  }

  "put /deposit/:uuid/metadata" should "reject invalid datasetmetadata.json" in {
    authMocker.expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/metadata",
      body = """{"title":"blabla"}""", // N.B: key should be plural
      headers = Seq(fooBarBasicAuthHeader)
    ) {
      status shouldBe BAD_REQUEST_400
      body shouldBe """invalid DatasetMetadata: don't recognize {"title":"blabla"}"""
    }
  }
}
