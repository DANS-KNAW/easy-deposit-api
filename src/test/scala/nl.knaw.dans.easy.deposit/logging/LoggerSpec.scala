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
import org.scalatra.test.scalatest.ScalatraSuite
import org.scalatra.{ ActionResult, Ok, ScalatraBase, ScalatraServlet }

class LoggerSpec extends TestSupportFixture with ServletFixture with ScalatraSuite {

  "custom loggers" should "override default loggers" in {
    val stringBuffer = new StringBuilder

    trait MyResponseLogFormatter extends ResponseLogFormatter {
      this: ScalatraBase =>
      // override formatResponseLog as you wish in here
    }

    trait MyResponseLogger extends AbstractResponseLogger with MyResponseLogFormatter {
      this: ScalatraBase =>

      override def logResponse(actionResult: ActionResult): Unit = {
        stringBuffer.append(formatResponseLog(actionResult))
        stringBuffer.append("\n")
      }
    }

    trait MyRequestLogFormatter extends RequestLogFormatter {
      this: ScalatraBase =>
      // override formatRequestLog as you wish in here
    }

    trait MyRequestLogger extends AbstractRequestLogger with MyRequestLogFormatter {
      this: ScalatraBase =>
      override def logRequest(): Unit = {
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
      val javaVersion = System.getProperty("java.version")
      val clientVersion = "4.5.3" // org.apache.httpcomponents dependency; may change when upgrading scalatra-scalatest
      val defaultHeaders = s"""Connection -> [keep-alive], Accept-Encoding -> [gzip,deflate], User-Agent -> [Apache-HttpClient/$clientVersion (Java/$javaVersion)], Host -> [localhost:$port]"""
      stringBuffer.toString() shouldBe
        s"""GET http://localhost:$port/ remote=**.**.**.1; params=[]; headers=[$defaultHeaders]
           |GET returned status=200; authHeaders=[Content-Type -> [text/plain;charset=UTF-8]]; actionHeaders=[]
           |""".stripMargin
    }
  }
}
