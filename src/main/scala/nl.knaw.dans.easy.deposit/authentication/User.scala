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

case class User(id: String,
                groups: Seq[String] = Seq.empty,
                isActive: Boolean
                // add arguments when required for a profile page
               ) {
}
object User {
  def apply(attributes: Map[String, Seq[String]]): User = {
    // For possible attribute keys see:
    // https://github.com/DANS-KNAW/dans.easy-test-users/blob/master/templates
    User(
      attributes.getOrElse("uid", Seq.empty)
        .headOption // mandatory: https://github.com/DANS-KNAW/dans.easy-ldap-dir/blob/f17c391/files/easy-schema.ldif#L83-L84
        .getOrElse(""),
      attributes.getOrElse("easyGroups", Seq.empty),
      attributes.getOrElse("dansState", Seq.empty)
        .contains("ACTIVE")
    )
  }
}