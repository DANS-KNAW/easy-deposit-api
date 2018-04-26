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

import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState.UserState
import org.json4s.native.JsonMethods.parse
import org.json4s.{ DefaultFormats, Formats }

import scala.util.{ Failure, Try }

/** A user with minimal properties: just for identification and authorisation */
case class AuthUser(id: String,
                    groups: Seq[String] = Seq.empty,
                    state: UserState
                   )

object AuthUser {
  object UserState extends Enumeration {
    type UserState = Value
    val

    /** The user has successfully registered, but has not validated the registration; the account cannot be used (yet). */
    REGISTERED,

    /** The user has a valid registration; the account can be used. */
    ACTIVE,

    /** The user is blocked; the account cannot be used. */
    BLOCKED
    = Value
  }

  def apply(attributes: Map[String, Seq[String]]): AuthUser = {
    AuthUser(
      attributes.getOrElse("uid", Seq.empty)
        .headOption // mandatory: https://github.com/DANS-KNAW/dans.easy-ldap-dir/blob/f17c391/files/easy-schema.ldif#L83-L84
        .getOrElse(""),
      attributes.getOrElse("easyGroups", Seq.empty),
      attributes.getOrElse("dansState", Seq.empty)
        .headOption.map(value => UserState.withName(value))
        .getOrElse(UserState.BLOCKED)
    )
  }

  private class ActiveAuthUser(id: String,
                               groups: Seq[String] = Seq.empty
                              ) extends AuthUser(id, groups, UserState.ACTIVE)

  private implicit val jsonFormats: Formats = new DefaultFormats {}

  def fromJson(input: String): Try[AuthUser] = {
    Try(parse(input).extract[ActiveAuthUser]).recoverWith { case t =>
      Failure(new Exception(s"parse error [${ t.getClass }: ${ t.getMessage }] for: $input", t))
    }
  }
}