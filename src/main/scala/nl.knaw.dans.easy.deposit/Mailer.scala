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

import java.io.{ InputStream, OutputStream, StringWriter }
import java.util.Properties

import better.files.{ File, StringExtensions }
import javax.activation.DataSource
import javax.mail.internet.{ MimeBodyPart, MimeMultipart }
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, UserInfo }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.lang.NotImplementedException
import org.apache.commons.mail.MultiPartEmail
import org.apache.velocity.app.VelocityEngine
import org.apache.velocity.{ Template, VelocityContext }

import scala.util.Try
import scala.xml.Elem

case class Mailer (smtpHost: String, fromAddress: String, bounceAddress: String, bccs: String, templateDir: File) extends DebugEnhancedLogging {

  private  val engine = new VelocityEngine(new Properties() {
    setProperty("file.resource.loader.path", templateDir.toJava.getAbsolutePath)
  }) {
    init()
  }
  private val htmlTemplate: Template = engine.getTemplate("depositConfirmation.html")
  private val txtTemplate = engine.getTemplate("depositConfirmation.txt")

  private def templateContext(to: UserInfo, dm: DatasetMetadata) = {
    new VelocityContext {
      put("displayName", to.displayName)
      put("datasetTitle", dm.titles.getOrElse(Seq.empty).headOption.getOrElse(""))
      put("myDatasetsUrl", "") // TODO
      put("doi", dm.doi.getOrElse(""))
    }
  }

  private def generate(template: Template, context: VelocityContext): String = {
    resource.managed(new StringWriter).acquireAndGet { writer =>
      template.merge(context, writer)
      val string = writer.getBuffer.toString
      logger.debug(string)
      string
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
    bccs.split(" +, +").filter(_.nonEmpty).foreach(email.addBcc)
    val context = templateContext(to, dm)
    logger.info("placeholder values: " + context.getKeys.map(key => context.get(key.toString)).mkString)
    email.setContent(new MimeMultipart("mixed") {
      addBodyPart(new MimeBodyPart() {
        setContent(generate(htmlTemplate, context), "text/html")
      })
      addBodyPart(new MimeBodyPart() {
        setText(generate(txtTemplate, context), "UTF-8")
      })
    })
    StreamedDataSource(agreement, "application/pdf", "agreement.pdf").attachTo(email)
    metadata.foreach { case (name, content) =>
      StreamedDataSource(content.serialize.inputStream, "text/xml", name).attachTo(email)
    }
    email.buildMimeMessage()
    email
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
