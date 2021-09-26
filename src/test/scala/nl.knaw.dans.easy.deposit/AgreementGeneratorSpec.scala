/*
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
package nl.knaw.dans.easy.deposit

import java.nio.charset.StandardCharsets
import java.util.UUID

import nl.knaw.dans.easy.deposit.Errors.GeneratorError
import nl.knaw.dans.easy.deposit.docs.dm.SchemedValue
import nl.knaw.dans.easy.deposit.docs.{ AgreementData, MinimalDatasetMetadata }
import okhttp3.mockwebserver.{ MockResponse, MockWebServer }
import org.scalatest.BeforeAndAfterAll
import scalaj.http.{ Http, HttpResponse }

import scala.util.{ Failure, Success }

class AgreementGeneratorSpec extends TestSupportFixture with BeforeAndAfterAll {

  private val data = AgreementData(
    defaultUserInfo,
    new MinimalDatasetMetadata().copy(license = Some(SchemedValue(
      Some("dcterms:URI"),
      Some("http://creativecommons.org/publicdomain/zero/1.0")
    ))),
  )

  // configure the mock server
  private val server = new MockWebServer
  private val test_server = "/generate/"

  private val generator = AgreementGenerator(Http, server.url(test_server).url(),"text/html")

  override protected def afterAll(): Unit = {
    server.shutdown()
    super.afterAll()
  }

  "generate" should "generate the correct JSON and obtain the PDF" in {
    server.enqueue {
      new MockResponse()
        .addHeader("Content-Type", "application/pdf")
        .setResponseCode(200)
        .setBody("mocked pdf")
    }
    generator.generate(data, UUID.randomUUID())
      .map(new String(_)) shouldBe Success("mocked pdf")

    val request = server.takeRequest()
    request.getRequestLine shouldBe s"POST $test_server HTTP/1.1"
    request.getBody.readString(StandardCharsets.UTF_8) shouldBe
      s"""{"depositor":{"name":"fullName","address":"","zipcode":"","city":"","country":"","organisation":"","phone":"","email":"does.not.exist@dans.knaw.nl"},"doi":"mocked-DOI","title":"Lorum ipsum","dateSubmitted":"2018-03-22","dateAvailable":"2018-07-30","accessCategory":"OPEN_ACCESS","license":"http://creativecommons.org/publicdomain/zero/1.0","sample":false,"agreementVersion":"4.0","agreementLanguage":"EN"}"""
  }

  it should "process errors correctly" in {
    server.enqueue {
      new MockResponse()
        .addHeader("Content-Type", "text/plain")
        .setResponseCode(400)
        .setBody("some error message")
    }
    val uuid = UUID.randomUUID()
    generator.generate(data, uuid) should matchPattern {
      case Failure(GeneratorError(msg, HttpResponse("some error message", 400, _)))
        if msg == s"Could not generate agreement for dataset $uuid" =>
    }
  }
}