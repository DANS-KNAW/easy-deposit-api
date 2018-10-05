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

import java.net.UnknownHostException
import java.util.{ Base64, TimeZone, UUID }

import better.files.File
import better.files.File._
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker.mockedAuthenticationProvider
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.easy.deposit.authentication.{ AuthConfig, AuthUser, AuthenticationProvider, TokenSupport }
import nl.knaw.dans.easy.deposit.docs.{ DDM, DepositInfo }
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.{ DateTime, DateTimeUtils, DateTimeZone }
import org.scalatest._
import org.scalatest.enablers.Existence

import scala.util.Failure
import scala.xml.SAXParseException

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfterEach {
  implicit def existenceOfFile[FILE <: better.files.File]: Existence[FILE] = _.exists

  lazy val testDir: File = currentWorkingDirectory / "target" / "test" / getClass.getSimpleName
  lazy val uuid: UUID = UUID.randomUUID()


  val minimalJsonString: String =
    """{
      |  "identifiers": [{"scheme":"id-type:DOI", "value":"mocked-DOI"}],
      |  "titles": ["Lorum ipsum"],
      |  "descriptions": ["dolor"],
      |  "dates": [
      |    { "scheme": "dcterms:W3CDTF", "value": "2018", "qualifier": "dcterms:created" },
      |    { "scheme": "dcterms:W3CDTF", "value": "2018", "qualifier": "dcterms:available" },
      |  ],
      |  "creators": [ { "initials": "B.A.R.", "surname": "Foo" } ],
      |  "accessRights": { "category": "OPEN_ACCESS" },
      |  "audiences": [ { "scheme": "blabla", "key": "D35200", "value": "some audience"} ]
      |}""".stripMargin

  def testResource(file: String): File = File(getClass.getResource(file))

  def getManualTestResource(file: String): String = {
    (testResource("/manual-test") / file).contentAsString
  }

  def clearTestDir(): Unit = {
    if (testDir.exists)
      testDir.delete().createDirectories()
  }

  val nowYMD = "2018-03-22"
  val now = s"${ nowYMD }T21:43:01.576"
  val nowUTC = s"${ nowYMD }T20:43:01Z"
  /** Causes DateTime.now() to return a predefined value. */
  DateTimeUtils.setCurrentMillisFixed(new DateTime(nowUTC).getMillis)
  DateTimeZone.setDefault(DateTimeZone.forTimeZone(TimeZone.getTimeZone("Europe/Amsterdam")))

  val fooBarBasicAuthHeader: String = authenticationHeader("foo", "bar")

  def authenticationHeader(username: String, password: String, authType: String = "Basic"): String = {
    val encoded = Base64.getEncoder.encodeToString(s"$username:$password".getBytes())
    s"$authType $encoded"
  }

  def minimalAppConfig: Configuration = {
    new Configuration("", new PropertiesConfiguration() {
      addProperty("deposits.stage.upload", testSubDir("stage-upload").toString())
      addProperty("deposits.stage", testSubDir("stage").toString())
      addProperty("deposits.drafts", testSubDir("drafts").toString())
      addProperty("deposits.submit-to", testSubDir("easy-ingest-flow-inbox").toString())
      addProperty("pids.generator-service", "http://piHostDoesNotExist")
      addProperty("users.ldap-url", "http://ldapHostDoesNotExist")
      addProperty("users.ldap-parent-entry", "-")
      addProperty("users.ldap-admin-principal", "-")
      addProperty("users.ldap-admin-password", "-")
      addProperty("users.ldap-user-id-attr-name", "-")
    })
  }

  def dansBag: DansBag = {
    val depositInfo = DepositInfo()
    val deposit = DepositDir(testDir, "foo", depositInfo.id)
    val depositDir = deposit.baseDir / "foo" / depositInfo.id.toString
    DansV0Bag.empty(depositDir / "bag").getOrElse(null)
  }

  private def testSubDir(drafts: String) = {
    (testDir / drafts)
      .delete(true)
      .createIfNotExists(asDirectory = true, createParents = true)
  }
  private class TokenSupportImpl() extends TokenSupport with AuthConfig {

    // required by AuthConfig but not by TokenSupport
    def getAuthenticationProvider: AuthenticationProvider = mockedAuthenticationProvider

    def getProperties: PropertiesConfiguration = new PropertiesConfiguration()
  }
  private val tokenSupport = new TokenSupportImpl()

  def jwtConfig: TokenConfig = tokenSupport.tokenConfig

  def createJWT(user: AuthUser): String = {
    tokenSupport.encodeJWT(user)
  }

  def assumeSchemaAvailable: Assertion = {
    assume(DDM.triedSchema match {
      case Failure(e: SAXParseException) if e.getCause.isInstanceOf[UnknownHostException] => false
      case _ => true
    })
  }
}
