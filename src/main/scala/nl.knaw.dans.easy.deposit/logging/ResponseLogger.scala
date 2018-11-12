package nl.knaw.dans.easy.deposit.logging

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.ActionResult

trait AbstractResponseLogger extends DebugEnhancedLogging with ResponseLogFormatter {
  implicit val responseLogger: AbstractResponseLogger = this

  def logResponse(actionResult: ActionResult)
                 (implicit request: HttpServletRequest,
                  response: HttpServletResponse): Unit
}
trait ResponseLogger extends DebugEnhancedLogging with AbstractResponseLogger {
  override def logResponse(actionResult: ActionResult)
                          (implicit request: HttpServletRequest,
                           response: HttpServletResponse): Unit = {
    logger.info(formatResponseLog(actionResult))
  }
}
