package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.authentication.TokenSupport._
import org.json4s.native.JsonMethods.parse
import org.json4s.{ DefaultFormats, Formats }
import pdi.jwt.JwtAlgorithm.HS256
import pdi.jwt.{ Jwt, JwtClaim }

import scala.util.{ Failure, Try }

trait TokenSupport {
  val config = TokenConfig()

  def encode(user: AuthUser): String = {
    // TODO Add audience, remote ip?
    val claim = JwtClaim(s"""{"uid":"${ user.id }"}""")
      .issuedNow
      .expiresIn(config.expiresIn)
    Jwt.encode(claim, config.secretKey, config.algorithm)
  }

  def toUser(token: String): Try[AuthUser] = {
    for {
      decoded <- Jwt.decode(token, config.secretKey, Seq(config.algorithm))
      parsed <- fromJson(decoded)
    } yield AuthUser(parsed.uid, isActive = true)
    // TODO user status might have changed since sign-up, retrieve status from ldap
  }

  def isValid(token: String): Boolean = {
    Jwt.isValid(token, config.secretKey, Seq(config.algorithm))
    // TODO check expiration / refresh ?
  }

  def validate(token: String): Try[Unit] = Try {
    Jwt.validate(token, config.secretKey, Seq(config.algorithm))
    // TODO check expiration / refresh ?
  }
}
object TokenSupport {
  case class TokenConfig[T](secretKey: String = "test", // TODO change type to SecretKey?
                            expiresIn: Int = 60 * 60, // one hour
                            algorithm: T = HS256
                           )
  private implicit val jsonFormats: Formats = new DefaultFormats() {}
  private case class Token(exp: Long, iat: Long, uid: String)
  private def fromJson(token: String) = Try(parse(token).extract[Token])
    .recoverWith { case t =>
      Failure(new Exception(s"parse error [${ t.getClass }: ${ t.getMessage }] for: $token", t))
    }
}
