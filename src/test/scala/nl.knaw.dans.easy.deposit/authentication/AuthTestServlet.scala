package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatra.{ Forbidden, Ok }

class AuthTestServlet(authProvider: AuthenticationProvider,
                      properties: PropertiesConfiguration,
                      tokenConfig: TokenConfig
                     ) extends AuthenticationSupport {

  override def getAuthenticationProvider: AuthenticationProvider = authProvider

  override def getProperties: PropertiesConfiguration = properties

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


