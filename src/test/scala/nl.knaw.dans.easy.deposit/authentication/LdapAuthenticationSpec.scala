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
import nl.knaw.dans.lib.error._
import org.scalamock.scalatest.MockFactory
import org.scalatest.exceptions.TestFailedException

import scala.util.{ Failure, Success }

class LdapAuthenticationSpec extends TestSupportFixture with MockFactory {

  private def wire(ldapMocker: LdapMocker) = new LdapAuthentication {
    override val authentication: Authentication = new Authentication {
      override val ldapUserIdAttrName: String = ""
      override val ldapParentEntry: String = ""
      override val ldapProviderUrl: String = "http://"
      override val ldapAdminPrincipal: String = "x"
      override val ldapAdminPassword: String = "y"

      override def getContext(connectionProperties: util.Hashtable[String, String]): LdapContext = {
        ldapMocker.mockedLdapContext
      }
    }
  }.authentication

  "getUser(user,password)" should "return an active user" in {
    val authentication = wire(new LdapMocker {
      expectLdapAttributes(new BasicAttributes() {
        put("dansState", "ACTIVE")
        put("uid", "someone")
      })
    })
    authentication.authenticate("someone", "somepassword") should matchPattern {
      case Success(Some(AuthUser("someone", UserState.active))) =>
    }
  }

  it should "return a blocked user" in {
    val authentication = wire(new LdapMocker {
      expectLdapAttributes(new BasicAttributes() {
        put("dansState", "BLOCKED")
        put("uid", "someone")
      })
    })
    authentication.authenticate("someone", "somepassword") should matchPattern {
      case Success(Some(AuthUser("someone", UserState.blocked))) =>
    }
  }

  it should "fail on other ldap problems" in {
    val authentication = wire(new LdapMocker {
      expectLdapSearch throwing new Exception("whoops")
      expectLdapClose
    })
    authentication.authenticate("someone", "somepassword") should matchPattern {
      // different logging than with AuthenticationException
      case Failure(e: Exception) if e.getMessage == "whoops" =>
    }
  }

  it should "return none for an invalid username or password" in {
    val authentication = wire(new LdapMocker {
      expectLdapSearch throwing new AuthenticationException()
    })
    authentication.authenticate("someone", "somepassword") shouldBe Success(None)
  }

  it should "fail without proper expectations" in {
    wire(new LdapMocker).authenticate("someone", "somepassword") should matchPattern {
      case Failure(e: TestFailedException) if e.getMessage().startsWith("Unexpected call") =>
    }
  }

  it should "not access ldap with a blank user" in pendingUntilFixed {
    wire(new LdapMocker).authenticate(" ", "somepassword") shouldBe Success(None)
  }

  it should "not access ldap with a blank password" in pendingUntilFixed {
    wire(new LdapMocker).authenticate("someone", " ") shouldBe Success(None)
  }

  "getUser(user)" should "ignore a second prefix" in {
    // schema defines attribute as single-value
    // https://github.com/DANS-KNAW/easy-app/blob/b8e64ead3b74e5d453ef895a083847b21fc225c4/lib-deprecated/dans-ldap/src/main/java/nl/knaw/dans/common/ldap/management/DANSSchema.java#L33-L37
    wire(new LdapMocker {
      expectLdapAttributes(new BasicAttributes() {
        put("uid", "foo")
        put("dansPrefixes", "van")
        get("dansPrefixes").add("den")
        put("sn", "Berg")
      })
    }).getUser("someone")
      .getOrRecover(e => fail(e))
      .prefix shouldBe Some("van")
  }

  // TODO figure out how to intercept and assert the logging
  //  for now test manually with 'trace':  https://github.com/DANS-KNAW/easy-deposit-api/blob/f1b9924/src/test/resources/logback.xml#L15
  it should "log a user without an email" in {
    wire(new LdapMocker {
      expectLdapAttributes(new BasicAttributes() {
        put("uid", "foo")
        put("displayName", "F. Bar")
      })
    }).getUser("someone")
      .getOrRecover(e => fail(e))
      .name shouldBe "F. Bar"
  }

  it should "log a user without a display name" in {
    wire(new LdapMocker {
      expectLdapAttributes(new BasicAttributes() {
        put("uid", "foo")
        put("mail", "does.not.exist@dans.knaw.nl")
      })
    }).getUser("someone")
      .getOrRecover(e => fail(e))
      .email shouldBe "does.not.exist@dans.knaw.nl"
  }
}
