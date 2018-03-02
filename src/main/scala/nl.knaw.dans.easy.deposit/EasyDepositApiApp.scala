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

class EasyDepositApiApp(configuration: Configuration) extends DebugEnhancedLogging
  with DraftsComponent
  with LdapAuthentication {
  val draftRoot: File = File(configuration.properties.getString("deposits.drafts"))

  override val authentication: Authentication = new Authentication {
    override val ldapUserIdAttrName: String = configuration.properties.getString("users.ldap-user-id-attr-name")
    override val ldapParentEntry: String = configuration.properties.getString("users.ldap-parent-entry")
    override val ldapProviderUrl: String = configuration.properties.getString("users.ldap-url")
    logger.info(s"Authentication: ldapProviderUrl = $ldapProviderUrl")
    logger.info(s"Authentication: ldapParentEntry = $ldapParentEntry")
    logger.info(s"Authentication: ldapUserIdAttrName = $ldapUserIdAttrName")
  }
}
