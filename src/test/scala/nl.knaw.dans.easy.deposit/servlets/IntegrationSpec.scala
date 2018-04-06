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


  s"put /deposit + metadata" should "succeed" ignore {
    // manual test stumbles on not-implemented at
    // https://github.com/DANS-KNAW/easy-deposit-api/blob/3771e59/src/main/scala/nl.knaw.dans.easy.deposit/DepositDir.scala#L117
    expectsUserFooBar

    val uuid = post(
      uri = s"/deposit",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      new String(bodyBytes)
    }

    put(
      uri = s"/deposit/$uuid/metadata",
      body = """{"blabla":"blabla"}""",
      headers = Seq(("Authorization", fooBarBasicAuthHeader))
    ) {
      status shouldBe NO_CONTENT_204
    }
  }
}
