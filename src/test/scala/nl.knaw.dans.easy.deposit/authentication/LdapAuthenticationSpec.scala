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

import java.util

import javax.naming.AuthenticationException
import javax.naming.directory.BasicAttributes
import javax.naming.ldap.LdapContext
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState
import org.scalamock.scalatest.MockFactory

import scala.util.Success

class LdapAuthenticationSpec extends TestSupportFixture with MockFactory {

  val ldapMocker = LdapMocker()

  private def wiring = new LdapAuthentication {
    override val authentication: Authentication = new Authentication {
      override val ldapUserIdAttrName: String = ""
      override val ldapParentEntry: String = ""
      override val ldapProviderUrl: String = "http://"
      override val ldapAdminPrincipal: String = "x"
      override val ldapAdminPassword: String = "y"

      override def getContext(connectionProperties: util.Hashtable[String, String]): LdapContext = {
        ldapMocker.mockedLdpContext
      }
    }
  }

  "getUser(user,password)" should "return an active user" in {
    ldapMocker.expectLdapAttributes(new BasicAttributes() {
      put("dansState", "ACTIVE")
      put("uid", "someone")
      put("easyGroups", "abc")
    })

    wiring.authentication.authenticate("someone", "somepassword") should matchPattern {
      case Some(AuthUser("someone", Seq("abc"), UserState.ACTIVE)) =>
    }
  }

  it should "return none for a blocked user" in {
    ldapMocker.expectLdapAttributes(new BasicAttributes() {
      put("dansState", "BLOCKED")
    })

    wiring.authentication.authenticate("someone", "somepassword") shouldBe None
  }

  it should "fail on other ldap problems" in {
    ldapMocker.expectLdapSearch throwing new Exception("whoops")

    wiring.authentication.authenticate("someone", "somepassword") should matchPattern {
      case None => // different logging than with AuthenticationException
    }
  }

  it should "return none for an invalid username or password" in {
    ldapMocker.expectLdapSearch throwing new AuthenticationException()

    wiring.authentication.authenticate("someone", "somepassword") shouldBe None
  }

  it should "not access ldap with a blank user" in {

    wiring.authentication.authenticate(" ", "somepassword") should matchPattern {
      case None =>
    }
  }

  it should "not access ldap with a blank password" in {

    wiring.authentication.authenticate("someone", " ") should matchPattern {
      case None =>
    }
  }

  "getUser(user)" should "return user properties" in {
    ldapMocker.expectLdapAttributes(new BasicAttributes() {
      put("uid", "foo")
      put("dansPrefixes", "van")
      get("dansPrefixes").add("den")
      put("sn", "Berg")
      put("easyGroups", "Archeology")
      get("easyGroups").add("History")
    })

    inside(wiring.authentication.getUser("someone")) {
      case Success(user) => // just sampling the result
        user("dansPrefixes").toArray shouldBe Array("van", "den")
        user("easyGroups").toArray shouldBe Array("Archeology", "History")
    }
  }
}
