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
import org.scalatra.auth.Scentry
import org.scalatra.test.scalatest.ScalatraSuite

class ServletsSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  val app: EasyDepositApiApp = new EasyDepositApiApp(new Configuration("test", new PropertiesConfiguration() {
    addProperty("users.ldap-url", "ldap://hostDoesNotExist")
    addProperty("deposits.drafts", s"$testDir/drafts")
  })) {
    override val authentication: Authentication = mock[Authentication]
  }

  addServlet(new EasyDepositApiServlet(app), "/deposit/*")
  addServlet(new AuthenticationServlet(app), "/auth/*")

  "get /" should "return 404 not found" in {
    (app.authentication.getUser(_: String, _: String)) expects(*, *) never()
    get("/") {
      status shouldBe NOT_FOUND_404
    }
  }

  "get /deposit without credentials" should "redirect to /auth/signin" in {
    (app.authentication.getUser(_: String, _: String)) expects(*, *) never()
    get("/deposit") {
      status shouldBe MOVED_TEMPORARILY_302
      body shouldBe ""
      header("Location") shouldBe s"$baseUrl/auth/signin"
      header("Content-Type") shouldBe "text/html;charset=UTF-8"
      Option(header("Set-Cookie")) shouldBe None
    }
  }

  "get /auth/signin" should "present a login form" in {
    (app.authentication.getUser(_: String, _: String)) expects(*, *) never()
    get("/auth/signin") {
      status shouldBe OK_200
      body should include("""<label for="login">""")
      Option(header("Set-Cookie")) shouldBe None
      header("Content-Type") shouldBe "text/html;charset=UTF-8"
    }
  }

  "get /deposit with valid cookie token" should "be ok" in {

    (app.authentication.getUser(_: String, _: String)) expects(*, *) never()
    get(
      uri = "/deposit",
      headers = Seq(("Cookie", s"${ Scentry.scentryAuthKey }=user001"))
    ) {
      status shouldBe OK_200
      body shouldBe "User(user001,List(),false) : EASY Deposit Api Service running..."
    }
  }

  "get /deposit with valid basic authentication" should "be ok" ignore {

    (app.authentication.getUser(_: String, _: String)) expects("foo", "bar") returning
      Some(User("foo", isActive = true))
    get(
      uri = "/deposit",
      headers = Seq(("Authorization", "Basic foo:bar"))
    ) {
      status shouldBe OK_200
      body shouldBe "EASY Deposit Api Service running..."
    }
  }
}
