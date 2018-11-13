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

import java.util.Collections.enumeration

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import org.scalamock.scalatest.MockFactory

import scala.collection.JavaConverters._

object Mocker extends MockFactory {

  def mockResponse(headers: Map[String, Seq[String]]): HttpServletResponse = {
    val response = mock[HttpServletResponse]

    headers.foreach { case (key: String, values: Seq[String]) =>
      response.getHeaders _ expects key anyNumberOfTimes() returning values.asJava
    }
    response.getHeaderNames _ expects() anyNumberOfTimes() returning headers.keys.toSeq.asJava
    response
  }

  def mockRequest(headers: Map[String, Seq[String]]): HttpServletRequest = {
    val request = mock[HttpServletRequest]

    headers.foreach { case (key: String, values: Seq[String]) =>
      request.getHeaders _ expects key anyNumberOfTimes() returning enumeration(values.asJava)
    }
    request.getHeaderNames _ expects() anyNumberOfTimes() returning enumeration(headers.keys.toSeq.asJava)
    request
  }
}
