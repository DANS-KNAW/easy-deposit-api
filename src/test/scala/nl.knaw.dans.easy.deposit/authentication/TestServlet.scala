package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus
import org.joda.time.DateTime
import org.scalatra.Ok

class TestServlet(authProvider: AuthenticationProvider,
                  properties: PropertiesConfiguration,
                  tokenConfig: TokenConfig
                 ) extends AuthenticationSupport {

  override def getAuthenticationProvider: AuthenticationProvider = authProvider

  override def getProperties: PropertiesConfiguration = properties

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
