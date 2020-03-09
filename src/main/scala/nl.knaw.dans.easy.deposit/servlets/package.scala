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

import java.io.{ EOFException, IOException }
import java.nio.file.{ Files, Path }
import java.util.UUID
import java.util.zip.ZipException

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.deposit.Errors.{ ArchiveException, ArchiveMustBeOnlyFileException, ConfigurationException, MalformedArchiveException }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.apache.commons.compress.archivers.{ ArchiveEntry, ArchiveInputStream }
import org.scalatra.servlet.FileItem
import org.scalatra.util.RicherString._
import resource.managed

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

// @formatter:off
/**
 * EasyDepositApiServlet    just I'm alive status, no authentication required
 * AbstractAuthServlet      supports basic authentication as wel as session cookies
 * |                        cookies are created as a response on valid basic authentication
 * |                        cookies are refreshed(=created again) as a response
 * |                        on a valid session cookie
 * |___ AuthServlet         should not refresh session cookies
 * |___ ProtectedServlet    requires refreshed session cookies
 *     |___ UserServlet
 *     |___ DepositServlet
 */
// @formatter:on
package object servlets extends DebugEnhancedLogging {

  private val zipExtRegexp = "g?z(ip)?"
  private val tarExtRegexp = "tar(.gz)?"
  val archiveExtRegexp = s".+[.]($zipExtRegexp|$tarExtRegexp)"
  private val pre = "(x-)?g?zip"
  private val post = "-compress(ed)?"
  private val zipContentTypeRegexp = s"(($pre($post)?)|(x$post))"
  private val tarContentTypeRegexp = s"x-g?tar$post"
  val archiveContentTypeRegexp = s"application/($zipContentTypeRegexp|$tarContentTypeRegexp)"

  val contentTypeJson: (String, String) = "content-type" -> "application/json;charset=UTF-8"
  val contentTypePlainText: (String, String) = "content-type" -> "text/plain;charset=UTF-8"

  implicit class RichArchiveInputStream(val archiveInputStream: ArchiveInputStream) extends AnyVal {

    def unpackPlainEntriesTo(targetDir: File, draftDeposit: => DepositDir, path: => Path, uploadName: String): Try[Unit] = {
      def extract(entry: ArchiveEntry): Try[Unit] = {
        if (!(targetDir / entry.getName).isChildOf(targetDir))
          Failure(MalformedArchiveException(s"Can't extract ${ entry.getName }"))
        else if (entry.isDirectory) {
          Try((targetDir / entry.getName).createDirectories())
        }
        else {
          logger.debug(s"[${ draftDeposit.id }] Extracting ${ entry.getName }")
          Try {
            (targetDir / entry.getName).parent.createDirectories() // in case a directory was not specified separately
            Files.copy(archiveInputStream, (targetDir / entry.getName).path)
            ()
          }.recoverWith { case e: Throwable =>
            logger.error(e.getMessage, e)
            Failure(ArchiveException(entry.getName))
          }
        }
      }

      @tailrec
      def cleanup(file: File): Unit = {
        // in case walk returns a parent after its children
        // a __MACOSX gets deleted because its content was deleted
        // it will simply not be a directory anymore and not cause trouble
        if (file.isDirectory && (file.isEmpty || file.name == "__MACOSX")) {
          logger.info(s"[$draftDeposit] cleaning up $file")
          file.delete()
          if (file.parent != targetDir)
            cleanup(file.parent)
        }
      }

      Try(Option(archiveInputStream.getNextEntry)) match {
        case Success(None) |
             Failure(_: EOFException) => Failure(MalformedArchiveException(s"No entries found."))
        case Failure(e: ZipException) =>
          // for example the tested: Unexpected record signature: 0X88B1F
          Failure(MalformedArchiveException(e.getMessage))
        case Failure(e: IOException) if e.getCause != null && e.getCause.isInstanceOf[IllegalArgumentException] =>
          // for example EASY-2619: At offset ..., ... byte binary number exceeds maximum signed long value
          Failure(MalformedArchiveException(draftDeposit.mailToDansMessage(
            linkIntro = s"Extracting file(s) to $path caused a problem: ${ e.getCause.getMessage }",
            bodyMsg =
              s"""Something went wrong while extracting file(s) to $path.
                 |Cause: ${ e.getCause.getMessage }
                 |Could you please investigate the issue?
                 |""".stripMargin,
            ref = draftDeposit.id.toString,
          )))
        case Failure(e) => Failure(e)
        case Success(Some(firstEntry: ArchiveEntry)) =>
          logger.info(s"[$draftDeposit] Extracting archive to $targetDir")
          for {
            _ <- extract(firstEntry)
            _ <- Stream
              .continually(archiveInputStream.getNextEntry) // TODO recover from ZipException and IllegalArgumentException
              .takeWhile(Option(_).nonEmpty)
              .map(extract)
              .failFastOr(Success(()))
            _ = targetDir.walk().foreach(cleanup)
          } yield ()
      }
    }
  }

  implicit class RichFileItem(val fileItem: FileItem) extends AnyVal {

    private def matchesEitherOf(extensionRegexp: String, contentTypeRegexp: String) = {
      fileItem.name.matches(extensionRegexp) || fileItem.contentType.exists(_.matches(contentTypeRegexp))
    }

    def isArchive: Boolean = {
      matchesEitherOf(archiveExtRegexp, archiveContentTypeRegexp)
    }

    private def getArchiveInputStream: Try[resource.ManagedResource[ArchiveInputStream]] = Try {
      val charSet = fileItem.charset.getOrElse("UTF8")
      if (matchesEitherOf(s".+[.]$tarExtRegexp", tarContentTypeRegexp))
        managed(new TarArchiveInputStream(fileItem.getInputStream, charSet))
      else managed(new ZipArchiveInputStream(fileItem.getInputStream, charSet, true, true))
    }

    /**
     * @param targetDir    target for the extracted files
     * @param draftDeposit info for error messages
     * @param path         location relative to uploadRoot of draftDeposit (info for error messages)
     * @return
     */
    def unpackPlainEntriesTo(targetDir: File, draftDeposit: => DepositDir, path: => Path): Try[Unit] = {
      getArchiveInputStream.flatMap(_.apply(_.unpackPlainEntriesTo(targetDir, draftDeposit, path, fileItem.name)))
    }
  }

  implicit class RichFileItems(val fileItems: BufferedIterator[FileItem]) extends AnyVal {

    def moveNonArchive(multiPartLocation: Option[String], targetDir: File, id: UUID): Try[Unit] = {
      multiPartLocation.map(location =>
        fileItems
          .map(moveIfNotAnArchive(_, File(location), targetDir, id))
          .failFastOr(Success(()))
      ).getOrElse(Failure(ConfigurationException("multipart.location is missing")))
    }

    private def moveIfNotAnArchive(srcItem: FileItem, multiPartLocation: File, targetDir: File, id: UUID): Try[Unit] = {
      val target = targetDir / srcItem.name
      logger.info(s"[$id] staging upload to $target, contentType = ${ srcItem.contentType }")
      if (srcItem.name.isBlank) Success(()) // skip form field without selected files
      else if (srcItem.isArchive) Failure(ArchiveMustBeOnlyFileException(srcItem))
      else Try {
        val f = UUID.randomUUID().toString

        // Try to move the (big) uploaded file to a known name,
        // otherwise write the in-memory content, depending on the MultipartConfig values of the servlet.
        srcItem.part.write(f)

        // now we can move the upload to the location we really want
        (multiPartLocation / f).moveTo(target)(CopyOptions.atomically)
      }
    }

    def nextArchiveIfOnlyOne: Try[Option[FileItem]] = {
      skipLeadingEmptyFormFields()
      if (!fileItems.headOption.exists(_.isArchive)) Success(None)
      else {
        val leadingArchiveItem = fileItems.next()
        skipLeadingEmptyFormFields()
        if (fileItems.hasNext)
          Failure(ArchiveMustBeOnlyFileException(leadingArchiveItem))
        else Success(Some(leadingArchiveItem))
      }
    }

    private def skipLeadingEmptyFormFields(): Unit = {
      // forward pointer after check
      while (fileItems.headOption.exists(_.name.isBlank)) { fileItems.next() }
    }
  }
}
