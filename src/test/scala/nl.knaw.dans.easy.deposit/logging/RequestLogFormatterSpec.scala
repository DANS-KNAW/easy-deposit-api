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

import javax.servlet.http.HttpServletRequest
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import org.scalatra.{ MultiParams, ScalatraServlet }

class RequestLogFormatterSpec extends TestSupportFixture with MockFactory {

  class DefaultTestServlet(params: MultiParams = mockParams,
                           override val request: HttpServletRequest = mockRequest
                          ) extends ScalatraServlet with RequestLogFormatter {

    // too complex to provide these values with a mocked request
    override def multiParams(implicit request: HttpServletRequest): MultiParams = params

    def log: String = formatRequestLog
  }

  "formatRequestLog" should "mask everything" in {
    new DefaultTestServlet().log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=**.**.**.78; params=[password -> [*****], login -> [*****]]; headers=[cookie -> [scentry.auth.default.user=******.**.**], HTTP_AUTHORIZATION -> [basic *****], foo -> [bar]]"
  }

  it should "mask nothing" in {

    val servlet = new DefaultTestServlet() with PlainRemoteAddress with PlainParameters with PlainHeaders {}
    servlet.log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.56.78; params=[password -> [secret], login -> [mystery]]; headers=[cookie -> [scentry.auth.default.user=abc456.pq.xy], HTTP_AUTHORIZATION -> [basic 123x_], foo -> [bar]]"
  }

  it should "mask nothing but cookie headers" in {
    val servlet = new DefaultTestServlet() with PlainRemoteAddress with PlainParameters with PlainAuthorizationHeader {}
    servlet.log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=12.34.56.78; params=[password -> [secret], login -> [mystery]]; headers=[cookie -> [scentry.auth.default.user=******.**.**], HTTP_AUTHORIZATION -> [basic 123x_], foo -> [bar]]"
  }

  it should "add a custom message to the log line" in {
    new DefaultTestServlet {
      override protected def formatRequestLog: String = {
        super.formatRequestLog + " custom message"
      }
    }.log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=**.**.**.78; params=[password -> [*****], login -> [*****]]; headers=[cookie -> [scentry.auth.default.user=******.**.**], HTTP_AUTHORIZATION -> [basic *****], foo -> [bar]] custom message"
  }

  it should "mask all headers in the same way" in {
    new DefaultTestServlet {
      override protected def formatHeaders(headers: HeaderMap): HeaderMap = {
        headers.map { case (key: String, _) => (key, Seq("*")) }
      }
    }.log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=**.**.**.78; params=[password -> [*****], login -> [*****]]; headers=[cookie -> [*], HTTP_AUTHORIZATION -> [*], foo -> [*]]"
  }

  it should "mask all headers, even their names" in {
    new DefaultTestServlet {
      override protected def headersToString(headers: HeaderMap): String = "*****"
    }
      .log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=**.**.**.78; params=[password -> [*****], login -> [*****]]; headers=*****"
  }

  it should "mask also a (case sensitive) custom header" in {
    new DefaultTestServlet {
      override protected def formatHeaders(headers: HeaderMap): HeaderMap = {
        super.formatHeaders(headers).map {
          case ("foo", _) => ("foo", Seq("***"))
          case kv => kv
        }
      }
    }.log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=**.**.**.78; params=[password -> [*****], login -> [*****]]; headers=[cookie -> [scentry.auth.default.user=******.**.**], HTTP_AUTHORIZATION -> [basic *****], foo -> [***]]"
  }

  it should "log the number of parameters (alias named fields in web forms), not their names/values" in {
    new DefaultTestServlet {
      override protected def parametersToString(params: MultiParams): String = params.size.toString
    }.log shouldBe
      "GET http://does.not.exist.dans.knaw.nl remote=**.**.**.78; params=2; headers=[cookie -> [scentry.auth.default.user=******.**.**], HTTP_AUTHORIZATION -> [basic *****], foo -> [bar]]"
  }

  "formatRemoteAddress" should "keep the last significant portion" in {
    new DefaultTestServlet {
      def formatAddress: String = formatRemoteAddress("12.34.56.78")
    }.formatAddress shouldBe "**.**.**.78"
  }

  private def mockParams: MultiParams = {
    Map[String, Seq[String]](
      "password" -> Seq("secret"),
      "login" -> Seq("mystery"),
    )
  }

  private def mockRequest: HttpServletRequest = {
    val request = Mocker.mockRequest(headers = Map(
      "cookie" -> Seq("scentry.auth.default.user=abc456.pq.xy"),
      "HTTP_AUTHORIZATION" -> Seq("basic 123x_"),
      "foo" -> Seq("bar")
    ))
    request.getMethod _ expects() anyNumberOfTimes() returning
      "GET"
    request.getRequestURL _ expects() anyNumberOfTimes() returning
      new StringBuffer("http://does.not.exist.dans.knaw.nl")
    request.getRemoteAddr _ expects() anyNumberOfTimes() returning
      "12.34.56.78"
    request
  }
}
