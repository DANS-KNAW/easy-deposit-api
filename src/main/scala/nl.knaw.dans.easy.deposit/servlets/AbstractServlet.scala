/**
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.deposit.servlets

import java.io.IOException

import javax.servlet.ServletException
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.logging.servlet.{ HeaderMap, MaskedLogFormatter, RequestLogFormatter }
import org.scalatra.ScalatraServlet

import scala.collection.JavaConverters._

trait AbstractServlet extends ScalatraServlet with RequestLogFormatter with MaskedLogFormatter with DebugEnhancedLogging {
  @throws[ServletException]
  @throws[IOException]
  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit = {

    // TODO RequestLogFormatter.formatRequestLog uses implicit scalatraContext.request which is null here
    //  make that implicit implicit an explicit implicit on formatRequestLog
    //  so we can provide req here explicitly to override scalatraContext.request
    val remote = formatRemoteAddress(Option(req.getRemoteAddr).getOrElse(""))
    val headers = formatHeaders(getHeaderMap(req))
    val params = formatParameters(multiParams(req)).mkString("[", ", ", "]")

    logger.info(s"receiving $req remote=$remote params=$params headers=$headers $request")
    super.service(req, resp)
  }

  private def getHeaderMap(request: HttpServletRequest): HeaderMap = {
    // this is a copy of a private method in nl.knaw.dans.lib.logging.servlet.RequestLogFormatter
    request.getHeaderNames.asScala.toSeq
      .map(name => name -> Option(request.getHeaders(name)).fold(Seq.empty[String])(_.asScala.toSeq))
      .toMap
  }
}
