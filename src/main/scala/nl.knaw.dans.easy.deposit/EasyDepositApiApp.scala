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
package nl.knaw.dans.easy.deposit

import better.files.File
import nl.knaw.dans.easy.deposit.authentication.LdapAuthentication
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.easy.deposit.components.DraftsComponent
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatra.CookieOptions
import pdi.jwt.algorithms.JwtHmacAlgorithm
import pdi.jwt.{ JwtAlgorithm, JwtOptions }

import scala.util.Try

class EasyDepositApiApp(configuration: Configuration) extends DebugEnhancedLogging
  with DraftsComponent
  with LdapAuthentication {
  private val properties: PropertiesConfiguration = configuration.properties

  private def toHmacAlgorithm(value: String): JwtHmacAlgorithm = {
    // TODO confine use of the library to TokenSupport for easy replacement in case of trouble with the library
    Try {
      JwtAlgorithm.fromString(value).asInstanceOf[JwtHmacAlgorithm]
    }.getOrRecover { t => throw new Exception(s"asymmetrical or unknown JwtHmacAlgorithm configured [$value]: $t") }
  }

  val draftRoot: File = File(properties.getString("deposits.drafts"))

  private val expiresIn: Int = properties.getInt("auth.cookie.expiresIn", 10)

  val tokenConfig = TokenConfig(
    secretKey = properties.getString("auth.jwt.secret.key", "test"), // TODO Change type to SecretKey? Really in application.properties?
    expiresIn = expiresIn, // seconds
    algorithm = toHmacAlgorithm(properties.getString("auth.jwt.hmac.algorithm", "HS256")),
    options = JwtOptions(leeway = 10 ) // JWT lives 10 seconds longer than cookie
  )
  logger.info(s"tokenConfig: $tokenConfig")

  val authCookieOptions: CookieOptions = CookieOptions(
    domain = "", // limits which server get the cookie // TODO by default the host who sent it?
    path = "/", // limits which route gets the cookie, TODO configure and/or from mounts in Service class
    maxAge = expiresIn, // seconds
    secure = false, // TODO true when service supports HTTPS to prevent browsers to send it over http
    httpOnly = true, // JavaScript can't get the cookie
    // version = 0 // obsolete? https://stackoverflow.com/questions/29124177/recommended-set-cookie-version-used-by-web-servers-0-1-or-2#29143128
  )
  logger.info(s"authCookieOptions: $authCookieOptions")

  override val authentication: Authentication = new Authentication {
    override val ldapUserIdAttrName: String = properties.getString("users.ldap-user-id-attr-name")
    override val ldapParentEntry: String = properties.getString("users.ldap-parent-entry")
    override val ldapProviderUrl: String = properties.getString("users.ldap-url")
    logger.info(s"Authentication: ldapProviderUrl = $ldapProviderUrl")
    logger.info(s"Authentication: ldapParentEntry = $ldapParentEntry")
    logger.info(s"Authentication: ldapUserIdAttrName = $ldapUserIdAttrName")
  }

  def getVersion: String = {
    configuration.version
  }
}
