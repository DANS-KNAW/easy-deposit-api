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
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, _ }
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class IntegrationSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  private val depositApiApp = new EasyDepositApiApp(minimalAppConfig)
  mountServlets(depositApiApp, mockedAuthenticationProvider)

  private val basicAuthentication: (String, String) = ("Authorization", fooBarBasicAuthHeader)

  s"scenario: /deposit/:uuid/metadata life cycle" should "return default dataset metadata" in {

    // create dataset
    expectsUserFooBar
    val uuid = post(uri = s"/deposit", headers = Seq(basicAuthentication)) {
      new String(bodyBytes)
    }
    val metadataURI = s"/deposit/$uuid/metadata"

    // create dataset metadata
    expectsUserFooBar
    put(metadataURI, headers = Seq(basicAuthentication),
        body = """{"blabla":"blabla"}""" // more variations in DepositDirSpec
    ) {
      status shouldBe NO_CONTENT_204
    }
    (testDir / "drafts" / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json").toJava should exist

    // get dataset metadata
    expectsUserFooBar
    get(metadataURI, headers = Seq(basicAuthentication)) {
      status shouldBe OK_200
      body shouldBe """{"privacySensitiveDataPresent":"unspecified","acceptLicenseAgreement":false}"""
    }

    // invalidate the metadata and try again
    val dd = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid))
    val mdFile = (dd.baseDir / "foo" / uuid.toString / "bag" / "metadata" / "dataset.json").write("---")
    expectsUserFooBar
    get(metadataURI, headers = Seq(basicAuthentication)) {
      status shouldBe INTERNAL_SERVER_ERROR_500
    }

    // remove the dataset and try another time
    mdFile.delete() // TODO replace with "GET deposit/:uuid" when implemented
    expectsUserFooBar
    get(metadataURI, headers = Seq(basicAuthentication)) {
      status shouldBe NOT_FOUND_404
    }
  }
}
