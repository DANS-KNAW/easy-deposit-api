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

import org.json4s.{ DefaultFormats, Formats }
import org.json4s.native.JsonMethods.parse

import scala.util.{ Failure, Try }

/** A user with minimal properties: just for identification and authorisation */
case class AuthUser(id: String,
                    groups: Seq[String] = Seq.empty,
                    roles: Seq[String] = Seq.empty,
                    isActive: Boolean // default false when from ldap, true when from token
                   )

object AuthUser {

  def apply(attributes: Map[String, Seq[String]]): AuthUser = {
    // For possible attribute keys see:
    // https://github.com/DANS-KNAW/dans.easy-test-users/blob/master/templates
    AuthUser(
      attributes.getOrElse("uid", Seq.empty)
        .headOption // mandatory: https://github.com/DANS-KNAW/dans.easy-ldap-dir/blob/f17c391/files/easy-schema.ldif#L83-L84
        .getOrElse(""),
      attributes.getOrElse("easyGroups", Seq.empty),
      attributes.getOrElse("easyRoles", Seq.empty),
      attributes.getOrElse("dansState", Seq.empty)
        .contains("ACTIVE")
    )
  }

  private class ActiveAuthUser(id: String,
                               groups: Seq[String] = Seq.empty,
                               roles: Seq[String] = Seq.empty
                              ) extends AuthUser(id, groups, roles, true)

  private implicit val jsonFormats: Formats = new DefaultFormats {}

  def fromJson(input: String): Try[AuthUser] = {
    Try(parse(input).extract[ActiveAuthUser]).recoverWith { case t =>
      Failure(new Exception(s"parse error [${ t.getClass }: ${ t.getMessage }] for: $input", t))
    }
  }
}