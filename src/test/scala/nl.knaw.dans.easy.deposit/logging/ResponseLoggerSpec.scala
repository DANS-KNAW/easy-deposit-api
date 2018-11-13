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
import org.scalatra.{ ActionResult, Ok, ScalatraBase, ScalatraServlet }

class ResponseLoggerSpec extends TestSupportFixture with MockFactory {

  class DefaultServlet extends ScalatraServlet with ResponseLogger {
    def test: ActionResult = Ok().logResponse
  }

  "custom response logger" should "override default logger" in {
    val stringBuffer = new StringBuilder

    trait MyLogger extends ResponseLogger {
      this: ScalatraBase =>

      override def logResponse(actionResult: ActionResult): Unit = {
        stringBuffer.append(formatResponseLog(actionResult))
      }
    }
    class MyServlet(implicit override val request: HttpServletRequest = mockRequest,
                    implicit override val response: HttpServletResponse = Mocker.mockResponse(headers = Map.empty)
                   ) extends DefaultServlet with MyLogger {}

    new MyServlet().test
    stringBuffer.toString() shouldBe "GET returned status=200; authHeaders=[]; actionHeaders=[]"
  }

  private def mockRequest: HttpServletRequest = {
    val request = Mocker.mockRequest(headers = Map.empty)
    request.getMethod _ expects() anyNumberOfTimes() returning "GET"
    request
  }
}
