package nl.knaw.dans.easy.deposit.servlets

import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging
import nl.knaw.dans.easy.deposit.authentication.ServletEnhancedLogging._
import org.scalatra.Forbidden

class ProtectedServlet(app: EasyDepositApiApp) extends AbstractAuthServlet(app) with ServletEnhancedLogging {

  before() {
    if (!isAuthenticated) {
      halt(Forbidden("missing, invalid or expired credentials").logResponse)
    }
  }
}
