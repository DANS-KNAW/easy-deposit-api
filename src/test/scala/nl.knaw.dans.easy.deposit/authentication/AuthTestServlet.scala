package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import org.scalatra.{ CookieOptions, Forbidden, Ok }

class AuthTestServlet(authProvider: AuthenticationProvider,
                      cookieOptions: CookieOptions,
                      tokenConfig: TokenConfig
                     ) extends AuthenticationSupport {

  // TODO apply `application.properies` here ?
  override def getAuthenticationProvider: AuthenticationProvider = authProvider

  override def getCookieOptions: CookieOptions = cookieOptions

  override def getTokenConfig: TokenConfig = tokenConfig

  post("/login") {
    if (isAuthenticated) {
      Ok(s"signed in")
    }
    else {
      Forbidden("invalid credentials")
    }
  }

  put("/logout") {
    logOut() // destroys the scentry cookie
    Ok("you are signed out")
  }
}


