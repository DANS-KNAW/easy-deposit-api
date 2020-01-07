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

import java.io.StringWriter
import java.net.URL
import java.util.{ Properties, UUID }

import better.files.File
import javax.activation.DataSource
import javax.mail.util.ByteArrayDataSource
import nl.knaw.dans.easy.deposit.Mailer.{ notEmpty, hasFileList }
import nl.knaw.dans.easy.deposit.docs.AgreementData
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.mail.{ HtmlEmail, MultiPartEmail }
import org.apache.velocity.app.VelocityEngine
import org.apache.velocity.{ Template, VelocityContext }

import scala.util.Try
import scala.xml.Elem

case class Mailer(smtpHost: String,
                  fromAddress: String,
                  bounceAddress: String,
                  bccs: Seq[String],
                  templateDir: File,
                  myDatasets: URL,
                 ) extends DebugEnhancedLogging {

  private val engine = new VelocityEngine(new Properties() {
    setProperty("runtime.references.strict", "true")
    setProperty("runtime.log.logsystem.class", "org.apache.velocity.runtime.log.NullLogChute")
    setProperty("file.resource.loader.path", templateDir.toJava.getAbsolutePath)
  }) {
    init()
  }
  private val htmlTemplate: Template = engine.getTemplate("depositConfirmation.html")
  private val txtTemplate = engine.getTemplate("depositConfirmation.txt")

  private def generate(template: Template, context: VelocityContext): String = {
    resource.managed(new StringWriter).acquireAndGet { writer =>
      template.merge(context, writer)
      writer.getBuffer.toString
    }
  }

  /** @return messageID */
  def buildMessage(data: AgreementData, attachments: Map[String, DataSource], depositId: UUID): Try[MultiPartEmail] = Try {
    val context = new VelocityContext {
      put("displayName", data.depositor.name)
      put("datasetTitle", data.title)
      put("myDatasetsUrl", myDatasets.toString)
      put("doi", data.doi)
      put("hasFileListAttached", hasFileList(attachments))
    }
    logger.info(s"[$depositId] email placeholder values: ${ context.getKeys.map(key => s"$key=${ context.get(key.toString) }").mkString(", ") }")
    val email = new HtmlEmail()
    email.setHtmlMsg(generate(htmlTemplate, context))
    email.setTextMsg(generate(txtTemplate, context))
    email.setSubject(s"DANS EASY: Deposit confirmation for ${ data.title }")
    email.addTo(data.depositor.email)
    email.setFrom(fromAddress)
    email.setBounceAddress(bounceAddress)
    bccs.foreach(email.addBcc)
    attachments.foreach { case (name, content) =>
      if (notEmpty(content))
        email.attach(content, name, name)
    }
    email.setHostName(smtpHost)
    email.buildMimeMessage()
    email
  }
}
object Mailer extends DebugEnhancedLogging {

  private def hasFileList(attachments: Map[String, DataSource]): Boolean = {
    attachments.exists { case (name, content) => name.startsWith("files.") && notEmpty(content) }
  }

  private def notEmpty(content: DataSource): Boolean = {
    content.getInputStream.available() > 0
  }

  def send(id: UUID, email: MultiPartEmail): Try[String] = {
    Try(email.sendMimeMessage)
      .map { messageId =>
        logger.info(s"[$id] sent email $messageId")
        messageId
      }
      .doIfFailure { case e => logger.error(s"[$id] could not send deposit confirmation message", e) }
  }

  def pdfDataSource(data: Array[Byte]): DataSource = {
    new ByteArrayDataSource(data, "application/pdf")
  }

  def xmlDataSource(data: Elem): DataSource = {
    xmlDataSource(data.serialize)
  }

  def xmlDataSource(data: String): DataSource = {
    new ByteArrayDataSource(data.getBytes, "text/xml")
  }

  def txtDataSource(data: String): DataSource = {
    new ByteArrayDataSource(data.getBytes, "text/plain")
  }
}
