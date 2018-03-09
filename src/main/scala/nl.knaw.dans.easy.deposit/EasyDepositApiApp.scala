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
import nl.knaw.dans.easy.deposit.components.DraftsComponent
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.CookieOptions

class EasyDepositApiApp(configuration: Configuration) extends DebugEnhancedLogging
  with DraftsComponent
  with LdapAuthentication {
  val draftRoot: File = File(configuration.properties.getString("deposits.drafts"))

  val authCookieOptions: CookieOptions = CookieOptions(
    domain = "", // limit who gets the cookie, TODO configure
    path = "/", // limit who gets the cookie, TODO configure and/or from mounts in Service class
    maxAge = 1000 * 60 * 60, // haven't seen firefox dropping a cookie after a while, TODO configure
    secure = false, // TODO true when service supports HTTPS to prevent browsers to send it over http
    httpOnly = true, // JavaScript can't get the cookie
    // version = 0 TODO obsolete? https://stackoverflow.com/questions/29124177/recommended-set-cookie-version-used-by-web-servers-0-1-or-2#29143128
  )
  logger.info(s"authCookieOptions: $authCookieOptions")


  override val authentication: Authentication = new Authentication {
    override val ldapUserIdAttrName: String = configuration.properties.getString("users.ldap-user-id-attr-name")
    override val ldapParentEntry: String = configuration.properties.getString("users.ldap-parent-entry")
    override val ldapProviderUrl: String = configuration.properties.getString("users.ldap-url")
    logger.info(s"Authentication: ldapProviderUrl = $ldapProviderUrl")
    logger.info(s"Authentication: ldapParentEntry = $ldapParentEntry")
    logger.info(s"Authentication: ldapUserIdAttrName = $ldapUserIdAttrName")
  }
}
