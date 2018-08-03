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

import java.util.{ Base64, TimeZone, UUID }

import better.files.File
import better.files.File._
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker.mockedAuthenticationProvider
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.easy.deposit.authentication.{ AuthConfig, AuthUser, AuthenticationProvider, TokenSupport }
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.{ DateTime, DateTimeUtils, DateTimeZone }
import org.scalatest._

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfterEach {

  lazy val testDir: File = currentWorkingDirectory / "target" / "test" / getClass.getSimpleName
  lazy val uuid: UUID = UUID.randomUUID()

  def testResource(file: String): File = File(getClass.getResource(file))

  def getManualTestResource(file: String): String = {
    (testResource("/manual-test") / file).contentAsString
  }

  def clearTestDir(): Unit = {
    if (testDir.exists)
      testDir.delete().createDirectories()
  }

  val now = "2018-03-22T21:43:01.576"
  val nowUTC = "2018-03-22T20:43:01Z"
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
      private val draftDir: File = (testDir / "drafts")
        .delete(true)
        .createIfNotExists(asDirectory = true, createParents = true)
      private val submitDir: File = (testDir / "easy-ingest-flow-inbox")
        .delete(true)
        .createIfNotExists(asDirectory = true, createParents = true)
      addProperty("deposits.drafts", draftDir.toString())
      addProperty("deposits.submit-to", submitDir.toString())
      addProperty("pids.generator-service", "http://hostDoesNotExist")
    })
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
}
