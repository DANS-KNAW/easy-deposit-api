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
package nl.knaw.dans.easy.deposit

import better.files.{ File, StringExtensions }
import nl.knaw.dans.easy.deposit.docs.dm.SchemedValue
import nl.knaw.dans.easy.deposit.docs.{ AgreementData, MinimalDatasetMetadata }

import scala.util.{ Failure, Success }

class MailerSpec extends TestSupportFixture {

  private val data = AgreementData(
    defaultUserInfo,
    new MinimalDatasetMetadata().copy(license = Some(SchemedValue(
      Some("dcterms:URI"),
      Some("http://creativecommons.org/publicdomain/zero/1.0")
    ))),
  )

  private val attachments = Map(
    "dataset.xml" -> <greeting>hello world</greeting>,
    "files.xml" -> <farewell>goodby world</farewell>,
  )

  "buildMessage" should "succeed" in {
//    new AgreementGenerator {
//      override val http: BaseHttp = null // TODO
//      override val url: URL = new URL( "http://deasy.dans.knaw.nl")
//    }.agreementDoc(data) shouldBe """"""
    val from = "does.not.exist@dans.knaw.nl"
    Mailer (smtpHost = "localhost", fromAddress = from, bounceAddress = from, bccs = "", templateDir = File("src/main/assembly/dist/cfg/template"))
      .buildMessage(data, "not implemented pdf".inputStream, attachments) shouldBe a[Success[_]]
  }

  "buildMessage" should "report invalid address" in {
    val from = "dans.knaw.nl"
    Mailer (smtpHost = "localhost", fromAddress = from, bounceAddress = from, bccs = "", templateDir = File("src/main/assembly/dist/cfg/template"))
      .buildMessage(data, "agreement".inputStream, attachments) should matchPattern {
      case Failure(e) if e.getMessage.matches(".*Missing final '@domain'.*dans.knaw.nl.*")=>
    }
  }

  "buildMessage" should "report missing templates" in pendingUntilFixed {
    val from = "does.not.exist@dans.knaw.nl"
    Mailer (smtpHost = "localhost", fromAddress = from, bounceAddress = from, bccs = "", templateDir = File("does/not/exist"))
      .buildMessage(data, "agreement".inputStream, attachments) should matchPattern {
      case Failure(e) if e.getMessage == "Unable to find resource 'depositConfirmation.html'" =>
    }
  }
}
