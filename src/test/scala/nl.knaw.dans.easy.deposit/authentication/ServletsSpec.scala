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
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class ServletsSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  val app: EasyDepositApiApp = new EasyDepositApiApp(new Configuration("test", new PropertiesConfiguration() {
    addProperty("users.ldap-url", "ldap://hostDoesNotExist")
    addProperty("deposits.drafts", s"$testDir/drafts")
  })) {
    override val authentication: Authentication = mock[Authentication]
  }

  addServlet(new EasyDepositApiServlet(app), "/deposit/*")
  addServlet(new AuthenticationServlet(app), "/auth/*")

  "get /deposit with valid basic authentication" should "be ok" ignore {
    // allows testing without curl without having to bake a (JWT) cookie
    // alternative: configure to accept some test-cookie or not

    (app.authentication.getUser(_: String, _: String)) expects("foo", "bar") returning
      Some(User("foo", isActive = true))
    get(
      uri = "/deposit",
      headers = Seq(("Authorization", "Basic foo:bar"))
    ) {
      // EasyBasicAuthStrategy.isValid logs true but stumbles in Scentry library class on:
      //    private[this] def _user(implicit request: HttpServletRequest): UserType =
      //      request.get(scentryAuthKey).orNull.asInstanceOf[UserType]
      status shouldBe OK_200
      body shouldBe "EASY Deposit Api Service running..."
    }
  }
}
