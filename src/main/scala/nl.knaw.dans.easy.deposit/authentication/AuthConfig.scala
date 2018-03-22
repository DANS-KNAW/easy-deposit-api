package nl.knaw.dans.easy.deposit.authentication

import org.apache.commons.configuration.PropertiesConfiguration

trait AuthConfig {

  def getAuthenticationProvider: AuthenticationProvider

  def getProperties: PropertiesConfiguration

}
