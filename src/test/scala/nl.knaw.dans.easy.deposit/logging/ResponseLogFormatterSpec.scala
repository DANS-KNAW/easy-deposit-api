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
package nl.knaw.dans.easy.deposit.logging

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import org.scalatra.{ ActionResult, Ok, ScalatraServlet }

class ResponseLogFormatterSpec extends TestSupportFixture with MockFactory {
  class DefaultTestServlet(implicit override val request: HttpServletRequest = mockRequest,
                           override val response: HttpServletResponse = mockResponse
                          ) extends ScalatraServlet with ResponseLogFormatter {

    def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
  }

  "ResponseLogFormatter" should "mask everything" in {
    val testServlet = new DefaultTestServlet()(mockRequest, mockResponse) {}
    testServlet.formatResponseLog(Ok()) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> [scentry.auth.default.user=******.**.**], REMOTE_USER -> [*****], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; actionHeaders=[]"
  }

  it should "not mask anything" in {
    val testServlet = new DefaultTestServlet()(mockRequest, mockResponse) with PlainAuthResponseHeaders {}
    testServlet.formatResponseLog(Ok(headers = Map("some" -> "header"))) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; actionHeaders=[some -> header]"
  }

  it should "not mask cookies" in {
    val testServlet = new DefaultTestServlet()(mockRequest, mockResponse) with PlainCookies {}
    testServlet.formatResponseLog(Ok(headers = Map("some" -> "header"))) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [*****], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; actionHeaders=[some -> header]"
  }

  private def mockResponse = {
    Mocker.mockResponse(headers = Map(
      "Set-Cookie" -> Seq("scentry.auth.default.user=abc456.pq.xy"),
      "REMOTE_USER" -> Seq("somebody"),
      "Expires" -> Seq("Thu, 01 Jan 1970 00:00:00 GMT"), // a date in the past means no cache for the returned content
    ))
  }

  private def mockRequest = {
    val req = mock[HttpServletRequest]
    req.getMethod _ expects() returning "GET" anyNumberOfTimes()
    req
  }
}
