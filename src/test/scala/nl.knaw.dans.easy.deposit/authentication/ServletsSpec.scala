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
package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit._
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.test.scalatest.ScalatraSuite

class ServletsSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  val depositApp: EasyDepositApiApp = new EasyDepositApiApp(new Configuration("test", new PropertiesConfiguration() {
    addProperty("users.ldap-url", "ldap://hostDoesNotExist")
    addProperty("deposits.drafts", s"$testDir/drafts")
  })) {
    override val authentication: Authentication = mock[Authentication]
  }

  addServlet(new EasyDepositApiServlet(depositApp), "/deposit/*")
  addServlet(new AuthenticationServlet(depositApp), "/auth/*")

  "get /deposit with valid basic authentication" should "be ok" ignore {
    // allows testing with curl without having to bake a (JWT) cookie
    // alternative: configure to accept some test-cookie or one of the test users

    (depositApp.authentication.getUser(_: String, _: String)) expects("foo", "bar") returning
      Some(AuthUser("foo", isActive = true))
    get(
      uri = "/deposit",
      headers = Seq(("Authorization", "Basic Zm9vOmJhcg=="))
    ) {
      // EasyBasicAuthStrategy.isValid logs true but stumbles in Scentry library class on:
      //    private[this] def _user(implicit request: HttpServletRequest): UserType =
      //      request.get(scentryAuthKey).orNull.asInstanceOf[UserType]
      body shouldBe "EASY Deposit Api Service running..."
      status shouldBe OK_200
    }
  }
}
