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

import java.io.{ ByteArrayOutputStream, InputStream, OutputStream, OutputStreamWriter }
import java.util.Properties

import better.files.{ File, StringExtensions }
import javax.activation.DataSource
import javax.mail.internet.{ MimeBodyPart, MimeMultipart }
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, UserInfo }
import org.apache.commons.lang.NotImplementedException
import org.apache.commons.mail.MultiPartEmail
import org.apache.velocity.VelocityContext
import org.apache.velocity.app.VelocityEngine

import scala.util.Try
import scala.xml.Elem

trait Mailer {
  val smtpHost: String
  val fromAddress: String
  val bounceAddress: String
  val bcc: String // internal copies for trouble shooting of automated mails
  val templateDir: File

  private lazy val engine = new VelocityEngine(new Properties() {
    setProperty("file.resource.loader.path", templateDir.toString)
  }) {
    init()
  }

  private def generate(extension: String, context: VelocityContext): String = {
    val encoding = "UTF-8"
    resource.managed(new ByteArrayOutputStream()).acquireAndGet{ outputStream =>
      new OutputStreamWriter(outputStream, encoding)
      val writer = new OutputStreamWriter(outputStream, encoding)
      engine.mergeTemplate(s"depositConfirmation.$extension", encoding, context,writer)
      val s = outputStream.toString
      s
    }
  }

  /** @return messageID */
  def buildMessage(to: UserInfo, dm: DatasetMetadata, agreement: InputStream, metadata: Map[String, Elem]): Try[MultiPartEmail] = Try {
    val email = new MultiPartEmail()
    email.setHostName(smtpHost)
    email.setSubject(s"DANS EASY: Deposit confirmation for ${ dm.titles.getOrElse("...") }")
    email.setFrom(fromAddress)
    email.setBounceAddress(bounceAddress)
    email.addTo(to.email)
    bcc.split(" +, +").filter(_.nonEmpty).foreach(email.addBcc)
    val context = createContext(to, dm)
    email.setContent(new MimeMultipart("mixed") {
      addBodyPart(new MimeBodyPart() {
        setContent(generate("html",context), "text/html")
      })
      addBodyPart(new MimeBodyPart() {
        setText(generate("txt",context), "UTF-8")
      })
    })
    StreamedDataSource(agreement, "application/pdf", "agreement.pdf").attachTo(email)
    metadata.foreach { case (name, content) =>
      StreamedDataSource(content.serialize.inputStream, "text/xml", name).attachTo(email)
    }
    // TODO streamed attachment from easy-deposit-agreement-generator
    email.buildMimeMessage()
    email
  }

  private def createContext(to: UserInfo, dm: DatasetMetadata) = {
    new VelocityContext {
      put("displayName",to.displayName)
      put("datasetTitle",dm.titles.getOrElse(""))
      put("myDatasetsUrl","")
      put("doi",dm.doi)
    }
  }

  private case class StreamedDataSource(content: InputStream, contentType: String, name: String) extends DataSource {
    override def getInputStream: InputStream = content

    override def getOutputStream: OutputStream = throw new NotImplementedException()

    override def getContentType: String = contentType

    override def getName: String = name

    def attachTo(email: MultiPartEmail): Unit = {
      email.attach(this, name, "", "attachment")
    }
  }

  def send(email: MultiPartEmail): Try[String] = Try {
    email.sendMimeMessage
  }
}
