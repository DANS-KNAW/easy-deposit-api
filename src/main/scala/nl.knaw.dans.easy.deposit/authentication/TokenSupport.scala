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

import nl.knaw.dans.easy.deposit.authentication.TokenSupport._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.native.JsonMethods.parse
import org.json4s.{ DefaultFormats, Formats }
import pdi.jwt.JwtAlgorithm.HS256
import pdi.jwt.algorithms.JwtHmacAlgorithm
import pdi.jwt.{ Jwt, JwtClaim, JwtOptions }

import scala.util.{ Failure, Try }

trait TokenSupport extends DebugEnhancedLogging {
  def getTokenConfig: TokenConfig

  def encode(user: AuthUser): String = {
    // TODO Add audience, remote ip?
    val claim = JwtClaim(s"""{"uid":"${ user.id }"}""")
      .issuedNow
      .expiresIn(getTokenConfig.expiresIn)
    Jwt.encode(claim, getTokenConfig.secretKey, getTokenConfig.algorithm)
  }

  def toUser(token: String): Try[AuthUser] = {
    trace("")
    for {
      decoded <- Jwt.decode(token, getTokenConfig.secretKey, Seq(getTokenConfig.algorithm))
      parsed <- fromJson(decoded)
    } yield AuthUser(parsed.uid, isActive = true)
    // TODO user status might have changed since sign-up, retrieve status from ldap
  }

  def isValid(token: String): Boolean = {
    trace("")
    Jwt.isValid(token, getTokenConfig.secretKey, Seq(getTokenConfig.algorithm))
    // TODO check expiration / refresh ?
  }

  def validate(token: String): Try[Unit] = Try {
    trace("")
    Jwt.validate(token, getTokenConfig.secretKey, Seq(getTokenConfig.algorithm))
    // TODO check expiration / refresh ?
  }
}
object TokenSupport {
  case class TokenConfig(secretKey: String = "test", // TODO change type to SecretKey?
                         expiresIn: Int = 60 * 60, // one hour
                         algorithm: JwtHmacAlgorithm = HS256,
                         options: JwtOptions = JwtOptions.DEFAULT
                        )
  private implicit val jsonFormats: Formats = new DefaultFormats() {}
  private case class Token(exp: Long, iat: Long, uid: String)
  private def fromJson(token: String) = Try(parse(token).extract[Token])
    .recoverWith { case t =>
      Failure(new Exception(s"parse error [${ t.getClass }: ${ t.getMessage }] for: $token", t))
    }
}
