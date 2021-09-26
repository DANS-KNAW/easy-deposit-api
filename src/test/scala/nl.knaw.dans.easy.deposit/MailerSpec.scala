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

import java.net.URL
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.docs.dm.SchemedValue
import nl.knaw.dans.easy.deposit.docs.{ AgreementData, MinimalDatasetMetadata }
import org.apache.velocity.exception.ResourceNotFoundException

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
    Mailer.agreementFileName("application/pdf") -> Mailer.dataSource("mocked pdf".getBytes, "application/pdf"),
    Mailer.datasetXmlAttachmentName -> Mailer.xmlDataSource(<greeting>hello world</greeting>.serialize),
    Mailer.filesAttachmentName -> Mailer.xmlDataSource(<farewell>goodby world</farewell>.serialize),
  )

  private val url = new URL("http:/localhost")
  private val validTemplateDir: File = File("src/main/assembly/dist/cfg/template")
  private val invalidTemplateDir: File = File("does/not/exist")
  "buildMessage" should "succeed" in {
    val from = "does.not.exist@dans.knaw.nl"
    Mailer(smtpHost = "localhost", fromAddress = from, bounceAddress = from, bccs = Seq.empty, validTemplateDir, url)
      .buildMessage(data, attachments, UUID.randomUUID(), msg4Datamanager = Option("")) shouldBe a[Success[_]]
  }

  it should "not add empty attachments" in {
    val from = "does.not.exist@dans.knaw.nl"
    val theseAttachments = attachments + (Mailer.filesAttachmentName -> Mailer.dataSource("".getBytes, "text/plain"))
    Mailer(smtpHost = "localhost", fromAddress = from, bounceAddress = from, bccs = Seq.empty, validTemplateDir, url)
      .buildMessage(data, theseAttachments, UUID.randomUUID(), msg4Datamanager = Option("")) shouldBe a[Success[_]]
  }

  it should "report invalid address" in {
    val from = "dans.knaw.nl"
    Mailer(smtpHost = "localhost", fromAddress = from, bounceAddress = from, bccs = Seq.empty, validTemplateDir, url)
      .buildMessage(data, attachments, UUID.randomUUID(), msg4Datamanager = Option("")) should matchPattern {
      case Failure(e) if e.getMessage.matches(".*Missing final '@domain'.*dans.knaw.nl.*") =>
    }
  }

  "constructor" should "report missing templates" in {
    val from = "does.not.exist@dans.knaw.nl"
    the[ResourceNotFoundException] thrownBy
      Mailer(smtpHost = "localhost", fromAddress = from, bounceAddress = from, bccs = Seq.empty, invalidTemplateDir, url) should
      have message "Unable to find resource 'depositConfirmation.html'"
  }
}
