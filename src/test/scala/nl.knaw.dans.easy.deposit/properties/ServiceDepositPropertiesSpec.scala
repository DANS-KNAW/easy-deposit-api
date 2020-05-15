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
package nl.knaw.dans.easy.deposit.properties

import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.properties.DepositProperties.SubmittedProperties
import nl.knaw.dans.easy.deposit.properties.graphql.GraphQLClient
import scalaj.http.{ BaseHttp, Http }
import okhttp3.HttpUrl
import okhttp3.mockwebserver.{ MockResponse, MockWebServer }
import org.scalatest.BeforeAndAfterAll
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.{ DefaultFormats, Formats }

import scala.util.{ Failure, Success }


class ServiceDepositPropertiesSpec extends TestSupportFixture with BeforeAndAfterAll {
  // configure the mock server
  private val server = new MockWebServer
  server.start()
  private val test_server = "/test_server/"
  private val baseUrl: HttpUrl = server.url(test_server)

  implicit val http: BaseHttp = Http
  implicit val formats: Formats = DefaultFormats
  private val client = new GraphQLClient(baseUrl.url())
  private val depositId = UUID.randomUUID()
  private val file: File = File("dummy")
  private val properties = new ServiceDepositProperties(file, depositId, client)

  override protected def afterAll(): Unit = {
    server.shutdown()
    super.afterAll()
  }

  "getSubmittedPropertiesFromService" should "return properties if the deposit exists" in {
    val response =
      """{
        |  "data": {
        |    "deposit": {
        |      "state": {
        |        "label": "ARCHIVED",
        |        "description": "deposit is archived"
        |      },
        |      "curationPerformed": {
        |        "value": "false"
        |      },
        |      "identifier": {
        |        "value": "easy-dataset:1"
        |      }
        |    }
        |  }
        |}""".stripMargin
    server.enqueue(new MockResponse().setBody(response))

    properties.getSubmittedPropertiesFromService shouldBe Success(Some(SubmittedProperties(depositId, "ARCHIVED", "deposit is archived", false, Some("easy-dataset:1"))))

    server.takeRequest().getBody.readUtf8() shouldBe Serialization.write {
      ("query" -> ServiceDepositProperties.GetSubmittedProperties.query) ~
        ("operationName" -> ServiceDepositProperties.GetSubmittedProperties.operationName) ~
        ("variables" -> Map("depositId" -> depositId.toString))
    }
  }

  it should "return CurationPerformed false if not set" in {
    val response =
      """{
        |  "data": {
        |    "deposit": {
        |      "state": {
        |        "label": "ARCHIVED",
        |        "description": "deposit is archived"
        |      },
        |      "curationPerformed": null
        |      "identifier": {
        |        "value": "easy-dataset:1"
        |      }
        |    }
        |  }
        |}""".stripMargin
    server.enqueue(new MockResponse().setBody(response))

    properties.getSubmittedPropertiesFromService shouldBe Success(Some(SubmittedProperties(depositId, "ARCHIVED", "deposit is archived", false, Some("easy-dataset:1"))))

    server.takeRequest().getBody.readUtf8() shouldBe Serialization.write {
      ("query" -> ServiceDepositProperties.GetSubmittedProperties.query) ~
        ("operationName" -> ServiceDepositProperties.GetSubmittedProperties.operationName) ~
        ("variables" -> Map("depositId" -> depositId.toString))
    }
  }

  it should "not return a fedoraIdentifier if not set" in {
    val response =
      """{
        |  "data": {
        |    "deposit": {
        |      "state": {
        |        "label": "ARCHIVED",
        |        "description": "deposit is archived"
        |      },
        |      "curationPerformed": null
        |      "identifier": null
        |    }
        |  }
        |}""".stripMargin
    server.enqueue(new MockResponse().setBody(response))

    properties.getSubmittedPropertiesFromService shouldBe Success(Some(SubmittedProperties(depositId, "ARCHIVED", "deposit is archived", false, None)))

    server.takeRequest().getBody.readUtf8() shouldBe Serialization.write {
      ("query" -> ServiceDepositProperties.GetSubmittedProperties.query) ~
        ("operationName" -> ServiceDepositProperties.GetSubmittedProperties.operationName) ~
        ("variables" -> Map("depositId" -> depositId.toString))
    }
  }

  it should "fail if the deposit does not exist" in {
    val response =
      """{
        |  "data": {
        |    "deposit": null
        |  }
        |}""".stripMargin
    server.enqueue(new MockResponse().setBody(response))

    properties.getSubmittedPropertiesFromService shouldBe Success(None)

    server.takeRequest().getBody.readUtf8() shouldBe Serialization.write {
      ("query" -> ServiceDepositProperties.GetSubmittedProperties.query) ~
        ("operationName" -> ServiceDepositProperties.GetSubmittedProperties.operationName) ~
        ("variables" -> Map("depositId" -> depositId.toString))
    }
  }
}
