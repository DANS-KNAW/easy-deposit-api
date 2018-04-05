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

import better.files.File
import better.files.File._
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker.mockedAuthenticationProvider
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.easy.deposit.authentication.{ AuthConfig, AuthUser, AuthenticationProvider, TokenSupport }
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.{ DateTime, DateTimeUtils }
import org.scalatest.{ BeforeAndAfter, FlatSpec, Inside, Matchers }

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfter {

  lazy val testDir: File = currentWorkingDirectory / "target" / "test" / getClass.getSimpleName

  def clearTestDir(): Unit = {
    if (testDir.exists)
      testDir.delete().createDirectories()
  }

  /** Causes DateTime.now() to return a predefined value. */
  def mockDateTimeNow(value: String): Unit = {
    DateTimeUtils.setCurrentMillisFixed(new DateTime(value).getMillis)
  }

  /** Base64 encoded foo:bar */
  val fooBarBasicAuthHeader = "Basic Zm9vOmJhcg=="

  def minimalAppConfig: Configuration = {
    new Configuration("", new PropertiesConfiguration() {
      addProperty("deposits.drafts", (testDir / "drafts")
        .delete(true)
        .createIfNotExists(asDirectory = true, createParents = true)
        .toString()
      )
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
