package nl.knaw.dans.easy.deposit.logging

import java.util

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import org.scalamock.scalatest.MockFactory

object Mocker extends MockFactory {

  def mockResponse(headers: Map[String, Seq[String]]): HttpServletResponse = {
    val response = mock[HttpServletResponse]

    headers.foreach { case (key: String, values: Seq[String]) =>
      response.getHeaders _ expects key anyNumberOfTimes() returning
        toVector(values)
    }
    response.getHeaderNames _ expects() anyNumberOfTimes() returning
      toVector(headers.keys.toSeq)
    response
  }

  def mockRequest(headers: Map[String, Seq[String]]): HttpServletRequest = {
    val request = mock[HttpServletRequest]

    headers.foreach { case (key: String, values: Seq[String]) =>
      request.getHeaders _ expects key anyNumberOfTimes() returning
        util.Collections.enumeration[String](toVector(values))
    }
    request.getHeaderNames _ expects() anyNumberOfTimes() returning
      toVector(headers.keys.toSeq).elements()
    request
  }

  private def toVector(seq: Seq[String]) = {
    val vector = new util.Vector[String]()
    seq.foreach(vector.add)
    vector
  }
}
