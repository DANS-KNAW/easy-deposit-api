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
package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.dm.AccessCategory.AccessCategory

object AccessCategory extends Enumeration {
  type AccessCategory = Value
  val open: AccessCategory = Value("OPEN_ACCESS")
  val openForRegisteredUsers: AccessCategory = Value("OPEN_ACCESS_FOR_REGISTERED_USERS")
  val restrictedGroup: AccessCategory = Value("GROUP_ACCESS")
  val restrictedRequest: AccessCategory = Value("REQUEST_PERMISSION")
  val otherAccess: AccessCategory = Value("NO_ACCESS")
  // TODO drop some as in https://github.com/DANS-KNAW/easy-deposit-ui/pull/81
}

case class AccessRights(category: AccessCategory,
                        group: Option[String], // TODO fix unit tests to drop this too
                       )
