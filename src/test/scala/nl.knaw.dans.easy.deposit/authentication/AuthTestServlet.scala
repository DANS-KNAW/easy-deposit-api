package nl.knaw.dans.easy.deposit.authentication

import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatra.{ Forbidden, Ok }

class AuthTestServlet(authProvider: AuthenticationProvider
                     ) extends AuthenticationSupport with AuthConfig {

  override def getAuthenticationProvider: AuthenticationProvider = authProvider

  override def getProperties: PropertiesConfiguration = new PropertiesConfiguration()

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


