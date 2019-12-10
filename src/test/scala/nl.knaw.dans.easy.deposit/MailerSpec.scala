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
import nl.knaw.dans.easy.deposit.docs.MinimalDatasetMetadata

import scala.util.{ Failure, Success }

class MailerSpec extends TestSupportFixture {
  "buildMessage" should "succeed" in {
    val metadata = Map(
      "dataset.xml" -> <greeting>hello world</greeting>,
      "files.xml" -> <farewell>goodby world</farewell>,
    )
    new Mailer {
      override val smtpHost: String = "localhost"
      override val fromAddress: String = "does.not.exist@dans.knaw.nl"
      override val bounceAddress: String = "does.not.exist@dans.knaw.nl"
      override val bcc: String = ""
      override val templateDir: File = File("src/main/assembly/dist/cfg/template")
    }.buildMessage(defaultUserInfo, new MinimalDatasetMetadata(), "agreement".inputStream, metadata) shouldBe a[Success[_]]
  }

  "buildMessage" should "report invalid address" in {
    val metadata = Map(
      "dataset.xml" -> <greeting>hello world</greeting>,
      "files.xml" -> <farewell>goodby world</farewell>,
    )
    new Mailer {
      override val smtpHost: String = "localhost"
      override val fromAddress: String = "dans.knaw.nl"
      override val bounceAddress: String = "dans.knaw.nl"
      override val bcc: String = ""
      override val templateDir: File = File("src/main/assembly/dist/cfg/template")
    }.buildMessage(defaultUserInfo, new MinimalDatasetMetadata(), "agreement".inputStream, metadata) should matchPattern {
      case Failure(e) if e.getMessage.matches(".*Missing final '@domain'.*dans.knaw.nl.*")=>
    }
  }

  "buildMessage" should "report missing templates" in {
    val metadata = Map(
      "dataset.xml" -> <greeting>hello world</greeting>,
      "files.xml" -> <farewell>goodby world</farewell>,
    )
    new Mailer {
      override val smtpHost: String = "localhost"
      override val fromAddress: String = "does.not.exist@dans.knaw.nl"
      override val bounceAddress: String = "does.not.exist@dans.knaw.nl"
      override val bcc: String = ""
      override val templateDir: File = File("does/not/exist")
    }.buildMessage(defaultUserInfo, new MinimalDatasetMetadata(), "agreement".inputStream, metadata) should matchPattern {
      case Failure(e) if e.getMessage == "Unable to find resource 'depositConfirmation.html'" =>
    }
  }
}
