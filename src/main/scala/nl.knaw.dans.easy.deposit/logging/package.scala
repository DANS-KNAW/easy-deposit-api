package nl.knaw.dans.easy.deposit

/**
 * Provides standard logging for servlet requests and responses masking sensitive data.
 *
 * ==Requests==
 *
 * All requests are logged at info level with:
 * {{{
 *   class MyServlet extends ScalatraServlet with RequestLogger {???}
 * }}}
 * To change the log level (or use another logger) define your own RequestLogger like:
 * {{{
 *   trait DebugRequestLogger extends RequestLogFormatter {
 *     this: ScalatraBase =>before() { logger.debug(formatRequestLog) }
 *   }
 * }}}
 * The [[nl.knaw.dans.easy.deposit.logging.RequestLogFormatter]] and unit tests
 * documents hooks to customize the content of the logged message.
 *
 * ==Responses==
 *
 * Each response must be logged explicitly by a method added to the class [[org.scalatra.ActionResult]].
 *
 * @example
 * {{{
 *
 *    package object servlets extends DebugEnhancedLogger {
 *      implicit val formatter: ResponseLogFormatter = new ResponseLogFormatter {}
 *      implicit class RichActionResult(val actionResult: ActionResult) extends AnyVal {
 *        def logResponse(implicit request: HttpServletRequest,
 *                        response: HttpServletResponse,
 *                        formatter: ResponseLogFormatter
 *                       ): ActionResult = {
 *          logger.info(formatter.logActionResult(actionResult))
 *          actionResult
 *        }
 *      }
 *    }
 *  }}}
 *
 * For flexible use of the customization hooks move the implicit instance of the
 * [[nl.knaw.dans.easy.deposit.logging.ResponseLogFormatter]]
 * formatter to the servlets and/or traits with "self: [[org.scalatra.ScalatraBase]]".
 * Making these instances private in traits allows the servlets to define their own formatting.
 *
 * ==Before and or after==
 *
 * The request are logged with before filters. An after filter would not see an [[org.scalatra.ActionResult]].
 * Its values are not saved in the implicit response provided by [[org.scalatra.ScalatraBase]]
 * as done by a trait for a [[org.scalatra.ScalatraServlet]] that extends [[org.scalatra.auth.ScentrySupport]].
 * See the last extensive readme version (documentation moved into an incomplete book and guides)
 * https://github.com/scalatra/scalatra/blob/6a614d17c38d19826467adcabf1dc746e3192dfc/README.markdown
 * sections #filters #action
 */
package object logging {

}
