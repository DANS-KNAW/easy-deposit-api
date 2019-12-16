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

import javax.naming.directory.{ Attribute, SearchControls, SearchResult }
import javax.naming.ldap.{ InitialLdapContext, LdapContext }
import javax.naming.{ AuthenticationException, Context }
import nl.knaw.dans.easy.deposit.docs.{ UserData, UserInfo }
import nl.knaw.dans.lib.error.TryExtensions
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import resource.managed

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

trait LdapAuthentication extends DebugEnhancedLogging {

  val authentication: AuthenticationProvider

  trait Authentication extends AuthenticationProvider {
    val ldapUserIdAttrName: String
    val ldapParentEntry: String
    val ldapProviderUrl: String
    val ldapAdminPrincipal: String
    val ldapAdminPassword: String
    val ldapUserClass = "easyUser"

    /**
     * @param userName name of the authenticated user
     * @return
     */
    def getUser(userName: String): Try[UserData] = {
      findUser(userName, adminContextProperties) match {
        case Success(Some(props)) => Success(UserData(props))
        case Success(None) => Failure(new Exception(s"User [$userName] not found by [$ldapAdminPrincipal] (deleted after login?)"))
        case Failure(t) => Failure(new Exception(s"Configuration error of ldap admin user: $t", t))
      }
    }

    def authenticate(userName: String, password: String): Try[Option[AuthUser]] = {
      findUser(userName, userContextProperties(userName, password))
        .doIfFailure { case t => logger.error(s"authentication of [$userName] failed with $t", t) }
        .map(_.map(AuthUser(_)))
    }

    private def findUser(searchedUserName: String, contextProperties: util.Hashtable[String, String]) = {
      val query = s"(&(objectClass=$ldapUserClass)($ldapUserIdAttrName=$searchedUserName))"
      val searchControls = new SearchControls() {
        setSearchScope(SearchControls.SUBTREE_SCOPE)
      }

      managed(getContext(contextProperties))
        .map(_
          .search(ldapParentEntry, query, searchControls)
          .asScala.toList.headOption
          .map(searchResult => toMap(searchResult))
        ).tried.recoverWith {
        case _: AuthenticationException => Success(None)
        case t => Failure(t)
      }
    }

    private def adminContextProperties = {
      createContextProperties(ldapAdminPrincipal, ldapAdminPassword)
    }

    private def userContextProperties(userName: String, password: String) = {
      createContextProperties(s"$ldapUserIdAttrName=$userName,$ldapParentEntry", password)
    }

    private def createContextProperties(principal: String, password: String) = {
      new util.Hashtable[String, String]() {
        put(Context.PROVIDER_URL, ldapProviderUrl)
        put(Context.SECURITY_AUTHENTICATION, "simple")
        put(Context.SECURITY_PRINCIPAL, principal)
        put(Context.SECURITY_CREDENTIALS, password)
        put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
      }
    }

    private def toMap(searchResult: SearchResult) = {
      searchResult.getAttributes.getAll.asScala.map(toTuple).toMap
    }

    private def toTuple(attribute: Attribute): (String, Seq[String]) = {
      (attribute.getID, attribute.getAll.asScala.map(_.toString).toSeq)
    }

    protected def getContext(contextProperties: util.Hashtable[String, String]): LdapContext = {
      new InitialLdapContext(contextProperties, null)
    }
  }
}
