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

import java.util.Properties

import better.files.File
import javax.mail.internet.{ MimeBodyPart, MimeMultipart }
import javax.mail.util.ByteArrayDataSource
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, UserInfo }
import org.apache.commons.mail.MultiPartEmail
import org.apache.velocity.app.VelocityEngine

import scala.util.Try

trait Mailer {
  val smtpHost: String
  val fromAddress: String
  val bounceAddress: String
  val bcc: String // internal copies for trouble shooting of automated mails
  val templateDir: File

  private lazy val textEngine = new VelocityEngine(new Properties() {
    setProperty("file.resource.loader.path", templateDir.toString)
    setProperty("template.file.name", "depositConfirmation.txt")
  }) {
    init()
  }
  private lazy val htmlEngine = new VelocityEngine(new Properties() {
    setProperty("file.resource.loader.path", templateDir.toString)
    setProperty("template.file.name", "depositConfirmation.html")
  }) {
    init()
  }

  /** @return messageID */
  def buildMessage(to: UserInfo, dm: DatasetMetadata, files: Map[String, String]): Try[MultiPartEmail] = Try {
    val email = new MultiPartEmail()
    email.setHostName(smtpHost)
    email.setSubject(s"DANS EASY: Deposit confirmation for ${ dm.titles.getOrElse("...") }")
    email.setFrom(fromAddress)
    email.setBounceAddress(bounceAddress)
    email.addTo(to.email)
    bcc.split(" +, +").filter(_.nonEmpty).foreach(email.addBcc)
    email.setContent(new MimeMultipart("mixed") {
      addBodyPart(new MimeBodyPart() {
        setContent(htmlContent(to, dm), "text/html")
      })
      addBodyPart(new MimeBodyPart() {
        setText(textContent(to, dm), "UTF-8")
      })
    })
    files.foreach { case (name, content) =>
      email.attach(xmlDataSource(content), name, "","attachment")
    }
    // TODO streamed attachment from easy-deposit-agreement-generator, see source code of ByteArrayDataSource
    email.buildMimeMessage()
    email
  }

  private def xmlDataSource(content: String) = {
    new ByteArrayDataSource(content.getBytes, "text/xml")
  }

  private def textContent(to: UserInfo, dm: DatasetMetadata) = {
    // TODO apply place holders to templates with velocity
    (templateDir / "depositConfirmation.txt").contentAsString
  }

  private def htmlContent(to: UserInfo, dm: DatasetMetadata) = {
    // TODO apply place holders to templates with velocity
    (templateDir / "depositConfirmation.html").contentAsString
  }

  def send(email: MultiPartEmail): Try[String] = Try {
    email.sendMimeMessage
  }
}
