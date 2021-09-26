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

import java.io.StringWriter
import java.net.URL
import java.nio.charset.StandardCharsets
import java.util.{ Properties, UUID }

import better.files.File
import javax.activation.DataSource
import javax.mail.util.ByteArrayDataSource
import nl.knaw.dans.easy.deposit.docs.AgreementData
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.mail.{ HtmlEmail, MultiPartEmail }
import org.apache.velocity.app.VelocityEngine
import org.apache.velocity.{ Template, VelocityContext }

import scala.util.Try

case class Mailer(smtpHost: String,
                  fromAddress: String,
                  bounceAddress: String,
                  bccs: Seq[String],
                  templateDir: File,
                  myDatasets: URL,
                  fileLimit: Int = 200,
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

  private def hasFileList(attachments: Map[String, DataSource]): Boolean = {
    attachments.exists { case (name, content) => name == Mailer.filesAttachmentName && notEmpty(content) }
  }

  private def notEmpty(content: DataSource): Boolean = {
    content.getInputStream.available() > 0
  }

  def buildMessage(data: AgreementData, attachments: Map[String, DataSource], depositId: UUID, msg4Datamanager: Option[String]): Try[MultiPartEmail] = Try {
    val context = new VelocityContext {
      put("displayName", data.depositor.name)
      put("datasetTitle", data.title)
      put("myDatasetsUrl", myDatasets.toString)
      put("doi", data.doi)
      put("hasFileListAttached", hasFileList(attachments))
      msg4Datamanager.foreach(put("msg4Datamanager", _))
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
    attachments.foreach {
      case (name, content) =>
        if (notEmpty(content))
          email.attach(content, name, name)
    }
    email.setHostName(smtpHost)
    email.buildMimeMessage()
    email
  }
}
object Mailer extends DebugEnhancedLogging {

  val datasetXmlAttachmentName = "metadata.xml"
  val filesAttachmentName = "files.txt"

  def agreementFileName(mimeType: String): String = {
    if (mimeType == "text/html")
      "agreement.html"
    else
      "agreement.pdf"
  }

  def dataSource(data: Array[Byte], mimeType: String): DataSource = {
    new ByteArrayDataSource(data, mimeType)
  }

  def xmlDataSource(data: String): DataSource = {
    dataSource(data.getBytes(StandardCharsets.UTF_8), "text/xml")
  }

  def txtDataSource(data: String): DataSource = {
    dataSource(data.getBytes(StandardCharsets.UTF_8), "text/plain")
  }
}
