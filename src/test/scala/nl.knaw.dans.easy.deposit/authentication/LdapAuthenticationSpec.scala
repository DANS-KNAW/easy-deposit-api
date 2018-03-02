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
import nl.knaw.dans.easy.deposit.authentication.LdapMocker._
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success }

class LdapAuthenticationSpec extends TestSupportFixture with MockFactory {

  private val wiring = new LdapAuthentication {
    override val authentication: Authentication = new Authentication {
      override val ldapUserIdAttrName: String = ""
      override val ldapParentEntry: String = ""
      override val ldapProviderUrl: String = "http://"

      override def getContext(connectionProperties: util.Hashtable[String, String]): LdapContext = {
        mockedLdpContext
      }
    }
  }

  "getUser" should "return an active user" in {
    expectLdapAttributes(new BasicAttributes() {
      put("dansState", "ACTIVE")
      put("uid", "someone")
      put("easyGroups", "abc")
    })

    inside(wiring.authentication.findUser("someone", "somepassword")) {
      case Success(Some(user)) => user.toString shouldBe "User(someone,Stream(abc, ?),true)"
    }
  }

  it should "return none for a blocked user" in {
    expectLdapAttributes(new BasicAttributes() {
      put("dansState", "BLOCKED")
    })

    wiring.authentication.findUser("someone", "somepassword") shouldBe Success(None)
  }

  it should "return none for an invalid username or password" in {
    expectLdapSearch throwing new AuthenticationException()

    wiring.authentication.findUser("someone", "somepassword") shouldBe Success(None)
  }

  it should "not access ldap with a blank user" in {

    inside(wiring.authentication.findUser(" ", "somepassword")) {
      case Failure(e: IllegalArgumentException) =>
    }
  }

  it should "not access ldap with a blank password" in {

    inside(wiring.authentication.findUser("someone", " ")) {
      case Failure(e: IllegalArgumentException) =>
    }
  }

  it should "fail on other ldap problems" in {
    expectLdapSearch throwing new Exception("whoops")

    inside(wiring.authentication.findUser("someone", "somepassword")) {
      case Failure(t) => t.getMessage shouldBe "whoops"
    }
  }
}
