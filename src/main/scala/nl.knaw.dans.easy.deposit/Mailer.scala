package nl.knaw.dans.easy.deposit

import java.util.{ Date, Properties }

import better.files.File
import javax.activation.{ DataHandler, FileDataSource }
import javax.mail.Message.RecipientType.{ BCC, TO }
import javax.mail._
import javax.mail.internet._
import nl.knaw.dans.easy.deposit.docs.{ DatasetMetadata, UserInfo }
import org.apache.velocity.app.VelocityEngine

import scala.util.Try

trait Mailer {
  val smtpHost: String
  val fromAddress: String
  val bcc: Option[String] // internal copies for trouble shooting of automated mails
  val templateDir: File

  lazy val textEngine = new VelocityEngine(new Properties() {
    setProperty("file.resource.loader.path", templateDir.toString())
    setProperty("template.file.name", "depositConfirmation.txt")
  }) {
    init()
  }
  lazy val htmlEngine = new VelocityEngine(new Properties() {
    setProperty("file.resource.loader.path", ???)
    setProperty("template.file.name", "depositConfirmation.html")
  }) {
    init()
  }

  def sendMessage(to: UserInfo, dm: DatasetMetadata, files: File*): Try[Unit] = Try {
    val subject = s"DANS EASY: Deposit confirmation for ${ dm.titles.headOption.getOrElse("...") }"
    val session = getSession
    Transport.send(new MimeMessage(session) {
      setSentDate(new Date())
      setSubject(subject)
      setFrom(new InternetAddress(fromAddress))
      setRecipients(BCC, parseRecipient(to.email))
      bcc.foreach(to => setRecipients(TO, parseRecipient(to)))
      setContent(new MimeMultipart("mixed") {
        // TODO apply place holders to templates with velocity
        addBodyPart(new MimeBodyPart() {
          setText((templateDir / "depositConfirmation.txt").contentAsString)
        })
        addBodyPart(new MimeBodyPart() {
          setContent((templateDir / "depositConfirmation.html").contentAsString, "text/html")
        })
        files.foreach(file => new MimeBodyPart() {
          setDataHandler(new DataHandler(new FileDataSource(file.toJava)))
          setFileName(file.name)
        })
      })
    })
  }

  protected def getSession = {

    Session.getInstance(new Properties() {
      put("mail.smtp.host", smtpHost)
    })
  }

  private def parseRecipient(to: String) = {
    InternetAddress.parse(to).asInstanceOf[Array[Address]]
  }
}
