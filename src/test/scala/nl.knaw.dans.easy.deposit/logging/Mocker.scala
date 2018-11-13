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
