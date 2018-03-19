package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import org.eclipse.jetty.http.HttpStatus
import org.joda.time.DateTime
import org.scalatra.{ CookieOptions, Ok }

class TestServlet(authProvider: AuthenticationProvider,
                  cookieOptions: CookieOptions,
                  tokenConfig: TokenConfig
                     ) extends AuthenticationSupport {

  override def getAuthenticationProvider: AuthenticationProvider = authProvider

  override def getCookieOptions: CookieOptions = cookieOptions

  override def getTokenConfig: TokenConfig = tokenConfig

  before() {
    if (!isAuthenticated) {
      halt(HttpStatus.FORBIDDEN_403, "missing, invalid or expired credentials")
    }
  }

  get("/") {
    contentType = "text/plain"
    Ok(s"$user ${ new DateTime() }: EASY Deposit API Service running")
  }
}
