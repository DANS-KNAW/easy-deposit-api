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

import java.util

import javax.servlet.http.HttpServletRequest
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import org.scalatra.{ MultiParams, ScalatraServlet }

class RequestLoggerSpec extends TestSupportFixture with MockFactory {

  class DefaultTestServlet(params: MultiParams) extends ScalatraServlet with RequestLogger {

    override def multiParams(implicit request: HttpServletRequest): MultiParams = params

    def log(request: HttpServletRequest): String = formatRequestLog(request)
  }

  "formatRequestLog" should "mask everything" in {
    new DefaultTestServlet(mockParams).log(mockRequest) shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.**.**; params=[password -> *****, login -> *****]; headers=[cookie -> scentry.auth.default.user=******.**.**, HTTP_AUTHORIZATION -> basic *****]"
  }

  it should "mask nothing" in {

    case class TestServlet() extends
      DefaultTestServlet(mockParams) with PlainRemoteAddress with PlainParameters with PlainHeaders
    TestServlet().log(mockRequest) shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.56.78; params=[password -> secret, login -> mystery]; headers=[cookie -> scentry.auth.default.user=abc456.pq.xy, HTTP_AUTHORIZATION -> basic 123x_]"
  }

  it should "mask nothing but cookie headers" in {

    case class TestServlet() extends
      DefaultTestServlet(mockParams) with PlainRemoteAddress with PlainParameters with PlainAuthorizationHeader
    TestServlet().log(mockRequest) shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.56.78; params=[password -> secret, login -> mystery]; headers=[cookie -> scentry.auth.default.user=******.**.**, HTTP_AUTHORIZATION -> basic 123x_]"
  }

  it should "add a custom message to the log line" in {

    case class TestServlet() extends DefaultTestServlet(mockParams) {
      override protected def formatRequestLog(implicit request: HttpServletRequest): String = {
        super.formatRequestLog(request) + " custom message"
      }
    }
    TestServlet().log(mockRequest) shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.**.**; params=[password -> *****, login -> *****]; headers=[cookie -> scentry.auth.default.user=******.**.**, HTTP_AUTHORIZATION -> basic *****] custom message"
  }

  it should "mask all headers" in {

    case class TestServlet() extends DefaultTestServlet(mockParams) {
      override protected def maskedHeadersToString(headers: Map[String, String]): String = "*****"
    }
    TestServlet().log(mockRequest) shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.**.**; params=[password -> *****, login -> *****]; headers=*****"
  }

  it should "mask all parameters" in {

    case class TestServlet() extends DefaultTestServlet(mockParams) {
      override protected def maskedParametersToString(headers: Map[String, String]): String = "*****"
    }
    TestServlet().log(mockRequest) shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.**.**; params=*****; headers=[cookie -> scentry.auth.default.user=******.**.**, HTTP_AUTHORIZATION -> basic *****]"
  }

  private def mockParams: MultiParams = {
    Map[String, Seq[String]](
      "password" -> Seq("secret"),
      "login" -> Seq("mystery"),
    )
  }

  private def mockRequest: HttpServletRequest = {
    val request = mock[HttpServletRequest]
    request.getMethod _ expects() returning
      "GET" anyNumberOfTimes()
    request.getRequestURL _ expects() returning
      new StringBuffer("http://does.not.exist.dans.knaw.nl") anyNumberOfTimes()
    request.getRemoteAddr _ expects() returning
      "12.34.56.78" anyNumberOfTimes()

    val headerMap = Map(
      "cookie" -> "scentry.auth.default.user=abc456.pq.xy",
      "HTTP_AUTHORIZATION" -> "basic 123x_",
      //"foo" -> "bar"
    )
    val headerNames = new util.Vector[String]()
    headerMap.foreach { case (key: String, value: String) =>
      headerNames.add(key)
      request.getHeader _ expects key returning value anyNumberOfTimes()
    }
    request.getHeaderNames _ expects() returning headerNames.elements() anyNumberOfTimes()
    request
  }
}
