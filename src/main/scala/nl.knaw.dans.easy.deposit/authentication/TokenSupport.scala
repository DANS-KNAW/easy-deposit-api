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
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.{ DefaultFormats, Formats }
import org.json4s.native.JsonMethods.parse
import pdi.jwt.{ Jwt, JwtAlgorithm, JwtClaim, JwtOptions }
import pdi.jwt.JwtAlgorithm.HS256
import pdi.jwt.algorithms.JwtHmacAlgorithm

import scala.util.{ Failure, Try }

trait TokenSupport extends DebugEnhancedLogging {
  self: AuthConfig => // TODO requires too much

  private def toHmacAlgorithm(value: String): JwtHmacAlgorithm = {
    Try {
      JwtAlgorithm.fromString(value).asInstanceOf[JwtHmacAlgorithm]
    }.getOrRecover { t => throw new Exception(s"asymmetrical or unknown JwtHmacAlgorithm configured [$value]: $t") }
  }

  val tokenConfig = TokenConfig(
    secretKey = getProperties.getString("auth.jwt.secret.key", "test"), // TODO Change type to SecretKey? Really in application.properties?
    expiresIn = getProperties.getInt("auth.cookie.expiresIn", 10), // seconds, MUST be same default as in AuthenticationSupport
    algorithm = toHmacAlgorithm(getProperties.getString("auth.jwt.hmac.algorithm", "HS256")),
    options = JwtOptions(leeway = 10) // JWT lives 10 seconds longer than cookie
  )
  logger.info(s"tokenConfig: $tokenConfig")

  def encodeJWT(user: AuthUser): String = {
    // TODO Add other user properties, audience=?=remote-ip?
    // Claim.content can only have primitive members, not even lists
    val claim = JwtClaim(s"""{"uid":"${ user.id }"}""")
      .issuedNow
      .expiresIn(tokenConfig.expiresIn)
    Jwt.encode(claim, tokenConfig.secretKey, tokenConfig.algorithm)
  }

  def decodeJWT(token: String): Try[AuthUser] = {
    for {
      decoded <- Jwt.decode(token, tokenConfig.secretKey, Seq(tokenConfig.algorithm))
      parsed <- fromJson(decoded)
    } yield AuthUser(parsed.uid, isActive = true)
    // TODO user status might have changed since sign-up, retrieve and check status from ldap
  }
}
object TokenSupport {
  case class TokenConfig(secretKey: String, // TODO change type to SecretKey?
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
