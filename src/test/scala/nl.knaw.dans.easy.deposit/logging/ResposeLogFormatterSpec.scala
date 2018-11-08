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

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import org.scalatra.Ok

class ResposeLogFormatterSpec extends TestSupportFixture with MockFactory {

  "ResponseLogFormatter" should "mask everything" in {
    new ResponseLogFormatter() {}
      .formatResponseLog(Ok())(mockRequest, mockResponse) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> scentry.auth.default.user=******.**.**, REMOTE_USER -> *****, Expires -> Thu, 01 Jan 1970 00:00:00 GMT]; actionHeaders=[]"
  }

  it should "not mask anything" in {
    new ResponseLogFormatter() with PlainAuthResponseHeaders {}
      .formatResponseLog(Ok(headers = Map("some" -> "header")))(mockRequest, mockResponse) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> scentry.auth.default.user=abc456.pq.xy, REMOTE_USER -> somebody, Expires -> Thu, 01 Jan 1970 00:00:00 GMT]; actionHeaders=[some -> header]"
  }

  it should "not mask cookies" in {
    new ResponseLogFormatter() with PlainCookieFormatter {}
      .formatResponseLog(Ok(headers = Map("some" -> "header")))(mockRequest, mockResponse) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> scentry.auth.default.user=abc456.pq.xy, REMOTE_USER -> *****, Expires -> Thu, 01 Jan 1970 00:00:00 GMT]; actionHeaders=[some -> header]"
  }

  private def mockResponse = {
    val response = mock[HttpServletResponse]

    val headerMap = Map(
      "Set-Cookie" -> "scentry.auth.default.user=abc456.pq.xy",
      "REMOTE_USER" -> "somebody",
      "Expires" -> "Thu, 01 Jan 1970 00:00:00 GMT", // a date in the past means no cache for the returned content
    )
    val headerNames = new util.Vector[String]()
    headerMap.foreach { case (key: String, value: String) =>
      headerNames.add(key)
      response.getHeader _ expects key returning value anyNumberOfTimes()
    }
    response.getHeaderNames _ expects() returning headerNames anyNumberOfTimes()
    response
  }

  private def mockRequest = {
    val req = mock[HttpServletRequest]
    req.getMethod _ expects() returning "GET" anyNumberOfTimes()
    req
  }
}
