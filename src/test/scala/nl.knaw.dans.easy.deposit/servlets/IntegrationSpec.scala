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

import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker._
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class IntegrationSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  private val depositApiApp = new EasyDepositApiApp(minimalAppConfig)
  mountServlets(depositApiApp, mockedAuthenticationProvider)

  s"scenario: POST /deposit; PUT+GET /deposit/:uuid/metadata" should "return default dataset metadata" in {

    expectsUserFooBar
    val uuid = post(
      uri = s"/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      new String(bodyBytes)
    }

    expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/metadata",
      body = """{"blabla":"blabla"}""", // more variations in DepositDirSpec
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe NO_CONTENT_204
    }
    (testDir / "drafts" / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json").toJava should exist

    expectsUserFooBar
    get(
      uri = s"/deposit/$uuid/metadata",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe OK_200
      body shouldBe """{"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
    }
  }
}
