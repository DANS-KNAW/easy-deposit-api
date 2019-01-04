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
/**
 * value1 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
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

  class TestServlet() extends ScalatraServlet {
    this: AbstractResponseLogger =>

    get("/") {
      contentType = "text/plain"
      Ok("How do you do?").logResponse
    }
  }
  val stringBuffer = new StringBuilder
  private val combinedLoggerPath = "/combinedLogger"
  private val requestLoggerPath = "/combinedLogger"
  private val customFormatterpath = "/combinedLogger"
  addServlet(new TestServlet() with CustomResponseLogger with CustomRequestLogger with ResponseLogFormatter with RequestLogFormatter, combinedLoggerPath)
  addServlet(new TestServlet() with CustomLoggers with ResponseLogFormatter with RequestLogFormatter, requestLoggerPath)
  addServlet(new TestServlet() with CustomLoggers with ResponseLogFormatter with CustomRequestLogFormatter, customFormatterpath)

  trait CustomResponseLogger extends AbstractResponseLogger {
    this: ScalatraBase with ResponseLogFormatter =>

    override def logResponse(actionResult: ActionResult): ActionResult = {
      stringBuffer.append(formatResponseLog(actionResult)).append("\n")
      actionResult
    }
  }

  trait CustomRequestLogger extends AbstractRequestLogger {
    this: ScalatraBase with RequestLogFormatter =>
    override def logRequest(): Unit = {
      stringBuffer.append(formatRequestLog).append("\n")
    }
  }

  trait CustomLoggers extends AbstractResponseLogger with AbstractRequestLogger {
    this: ScalatraBase with ResponseLogFormatter with RequestLogFormatter =>

    override def logResponse(actionResult: ActionResult): ActionResult = {
      stringBuffer.append(formatResponseLog(actionResult)).append("\n")
      actionResult
    }

    override def logRequest(): Unit = {
      stringBuffer.append(formatRequestLog).append("\n")
    }
  }

  trait CustomRequestLogFormatter extends RequestLogFormatter with PlainRemoteAddress {
    this: ScalatraBase =>
    // or other overrides (explicit and through traits)
    // are documented in XxxLogFormatterSpec
  }

  "separate custom loggers" should "override default loggers" in {
    shouldDivertLogging(combinedLoggerPath)
  }

  "combined custom loggers" should "override default loggers" in {
    shouldDivertLogging(requestLoggerPath)
  }

  "custom request formatter" should "alter logged content" in {
    shouldDivertLogging(customFormatterpath, formattedRemote = "127.0.0.1")
  }

  private def shouldDivertLogging(path: String, formattedRemote: String = "**.**.**.1") = {
    stringBuffer.clear()
    get(uri = path) {
      body shouldBe "How do you do?"
      status shouldBe OK_200
      val port = localPort.getOrElse("None")
      val loggedLines = stringBuffer.toString().split("\n")
      loggedLines.head should startWith(s"GET http://localhost:$port$path")
      loggedLines.last should startWith(s"GET returned status=200; ")
      loggedLines.last.toLowerCase() should include(s"content-type -> [text/plain;charset=utf-8]")
      loggedLines.last should include(s"actionHeaders=[]")
    }
  }
}
