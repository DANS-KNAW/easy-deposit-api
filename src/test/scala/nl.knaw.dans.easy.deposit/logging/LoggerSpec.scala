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

import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.servlets.ServletFixture
import org.eclipse.jetty.http.HttpStatus.OK_200
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite
import org.scalatra.{ ActionResult, Ok, ScalatraBase, ScalatraServlet }

class LoggerSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  "custom loggers" should "override default loggers" in {
    val stringBuffer = new StringBuilder

    trait MyResponseLogger extends ResponseLogger {
      this: ScalatraBase =>

      override def logResponse(actionResult: ActionResult): Unit = {
        stringBuffer.append(formatResponseLog(actionResult))
        stringBuffer.append("\n")
      }
    }

    trait MyRequestLogger extends RequestLogFormatter {
      this: ScalatraBase =>
      before() {
        stringBuffer.append(formatRequestLog)
        stringBuffer.append("\n")
      }
    }

    class MyServlet() extends ScalatraServlet with MyResponseLogger with MyRequestLogger {
      get("/") {
        contentType = "text/plain"
        Ok("How do you do?").logResponse
      }
    }
    addServlet(new MyServlet(), "/*")

    get(uri = "/") {
      body shouldBe "How do you do?"
      status shouldBe OK_200
      val port = localPort.getOrElse("None")
      stringBuffer.toString() shouldBe
        s"""GET http://localhost:$port/ remote=**.**.**.1; params=[]; headers=[Connection -> [keep-alive], Accept-Encoding -> [gzip,deflate], User-Agent -> [Apache-HttpClient/4.5.3 (Java/1.8.0_45)], Host -> [localhost:$port]]
           |GET returned status=200; authHeaders=[Content-Type -> [text/plain;charset=UTF-8]]; actionHeaders=[]
           |""".stripMargin
    }
  }
}
