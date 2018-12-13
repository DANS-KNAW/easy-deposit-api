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

import java.util.{ TimeZone, UUID }

import better.files.File
import better.files.File._
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.easy.deposit.authentication.{ AuthConfig, AuthUser, AuthenticationProvider, TokenSupport }
import nl.knaw.dans.easy.deposit.docs._
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.{ DateTime, DateTimeUtils, DateTimeZone }
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.enablers.Existence

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfterEach with MockFactory {
  implicit def existenceOfFile[FILE <: better.files.File]: Existence[FILE] = _.exists

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

  val nowYMD = "2018-03-22"
  val now = s"${ nowYMD }T21:43:01.576"
  val nowUTC = s"${ nowYMD }T20:43:01Z"
  /** Causes DateTime.now() to return a predefined value. */
  DateTimeUtils.setCurrentMillisFixed(new DateTime(nowUTC).getMillis)
  DateTimeZone.setDefault(DateTimeZone.forTimeZone(TimeZone.getTimeZone("Europe/Amsterdam")))

  def minimalAppConfig: Configuration = {
    new Configuration("", new PropertiesConfiguration() {
      addProperty("deposits.stage.upload", testSubDir("stage-upload").toString())
      addProperty("deposits.stage", testSubDir("stage").toString())
      addProperty("deposits.drafts", testSubDir("drafts").toString())
      addProperty("deposits.submit-to", testSubDir("easy-ingest-flow-inbox").toString())
      addProperty("pids.generator-service", "http://pidHostDoesNotExist.dans.knaw.nl")
      addProperty("users.ldap-url", "http://ldapHostDoesNotExist.dans.knaw.nl")
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
    def getAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]

    def getProperties: PropertiesConfiguration = new PropertiesConfiguration()
  }
  private val tokenSupport = new TokenSupportImpl()

  def jwtConfig: TokenConfig = tokenSupport.tokenConfig

  def createJWT(user: AuthUser): String = {
    tokenSupport.encodeJWT(user)
  }
}
