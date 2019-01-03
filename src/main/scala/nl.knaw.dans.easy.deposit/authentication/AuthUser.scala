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
                    state: UserState
                   )

object AuthUser {
  object UserState extends Enumeration {
    type UserState = Value

    /** The user has successfully registered, but has not validated the registration; the account cannot be used (yet). */
    val registered: Value = Value("REGISTERED")

    /** The user has a valid registration; the account can be used. */
    val active: Value = Value("ACTIVE")

    /** The user is blocked; the account cannot be used. */
    val blocked: Value = Value("BLOCKED")
  }

  def apply(attributes: Map[String, Seq[String]]): AuthUser = {
    AuthUser(
      attributes.getOrElse("uid", Seq.empty)
        .headOption // mandatory: https://github.com/DANS-KNAW/dans.easy-ldap-dir/blob/f17c391/files/easy-schema.ldif#L83-L84
        .getOrElse(""),
      attributes.getOrElse("dansState", Seq.empty)
        .headOption.map(value => UserState.withName(value))
        .getOrElse(UserState.blocked)
    )
  }

  private class ActiveAuthUser(id: String) extends AuthUser(id, UserState.active)

  private implicit val jsonFormats: Formats = new DefaultFormats {}

  def fromJson(input: String): Try[AuthUser] = {
    Try(parse(input).extract[ActiveAuthUser]).recoverWith { case t =>
      Failure(new Exception(s"parse error [$t] for: $input", t))
    }
  }
}
