package nl.knaw.dans.easy.deposit.logging

import java.util

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import org.scalamock.scalatest.MockFactory

object Mocker extends MockFactory {

  def mockResponse(headers: Map[String, Seq[String]]): HttpServletResponse = {
    val response = mock[HttpServletResponse]

    val headerNames = new util.Vector[String]()
    headers.foreach { case (key: String, values: Seq[String]) =>
      headerNames.add(key)
      val headerValues = new util.Vector[String]()
      values.foreach(headerValues.add)
      response.getHeaders _ expects key anyNumberOfTimes() returning headerValues
    }
    response.getHeaderNames _ expects() anyNumberOfTimes() returning headerNames
    response
  }

  def mockRequest(headers: Map[String, Seq[String]]): HttpServletRequest = {
    val request = mock[HttpServletRequest]

    val headerNames = new util.Vector[String]()
    headers.foreach { case (key: String, values: Seq[String]) =>
      headerNames.add(key)
      val headerValues = new util.Vector[String]()
      values.foreach(headerValues.add)
      request.getHeaders _ expects key anyNumberOfTimes() returning
        util.Collections.enumeration[String](headerValues)
    }
    request.getHeaderNames _ expects() anyNumberOfTimes() returning
      headerNames.elements()
    request
  }
}
