package nl.knaw.dans.easy.deposit.logging

import java.util

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import org.scalatra.Ok

class ResposeLogFormatterSpec extends TestSupportFixture with MockFactory {

  case class LogTester() extends ResponseLogFormatter
  "ResponseLogFormatter" should "mask authentication headers" in {
    new ResponseLogFormatter() {}
      .formatResponseLog(Ok())(mockRequest, mockResponse) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> scentry.auth.default.user=******.**.**, REMOTE_USER -> *****, Expires -> Thu, 01 Jan 1970 00:00:00 GMT]; actionHeaders=[]"
  }

  it should "not mask anything" in {
    new ResponseLogFormatter() with PlainAuthResponseHeaders {}
      .formatResponseLog(Ok(headers = Map("some"->"header")))(mockRequest, mockResponse) shouldBe
      "GET returned status=200; authHeaders=[Set-Cookie -> scentry.auth.default.user=abc456.pq.xy, REMOTE_USER -> somebody, Expires -> Thu, 01 Jan 1970 00:00:00 GMT]; actionHeaders=[some -> header]"
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
