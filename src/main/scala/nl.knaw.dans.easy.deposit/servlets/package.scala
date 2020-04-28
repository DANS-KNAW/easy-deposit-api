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
import nl.knaw.dans.easy.deposit.Errors.{ ArchiveException, ArchiveMustBeOnlyFileException, ConfigurationException, MalformedArchiveException, escape, printableRegexp }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import org.apache.commons.compress.archivers.tar.{ TarArchiveInputStream, TarConstants }
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.apache.commons.compress.archivers.{ ArchiveEntry, ArchiveInputStream }
import org.scalatra.servlet.FileItem
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

    /**
     *
     * @param targetDir    temporary file to stage uploads
     * @param draftDeposit deposit receiving the uploads (info for error message)
     * @param path         relative path for the upload root (info for error message)
     * @param uploadName   of multipart item of request (client point of view, info for error message)
     * @return
     */
    def unpackPlainEntriesTo(targetDir: File, draftDeposit: => DepositDir, path: => Path, uploadName: String): Try[Unit] = {
      def extractOne(entry: ArchiveEntry): Try[Unit] = {
        lazy val clientMsg = s"Can't extract ${ entry.getName } from $uploadName"
        val targetFile = targetDir / entry.getName
        if (!targetFile.isChildOf(targetDir)) {
          Failure(MalformedArchiveException(uploadName, entry.getName, path, "Invalid path"))
        }
        else if (entry.isDirectory) {
          Try(targetFile.createDirectories())
        }
        else {
          logger.debug(s"[${ draftDeposit.id }] Extracting ${ entry.getName } from $uploadName")
          Try {
            targetFile.parent.createDirectories() // in case a directory was not specified separately
            Files.copy(archiveInputStream, targetFile.path)
            ()
          }.recoverWith {
            case e: ZipException => Failure(MalformedArchiveException(uploadName, entry.getName, path, escape(e.getMessage)))
            case e: Throwable => Failure(ArchiveException(clientMsg, e))
          }
        }
      }

      @tailrec
      def cleanup(file: File): Unit = {
        // in case walk returns a parent after its children
        // a __MACOSX gets deleted because its content was deleted
        // it will simply not be a directory anymore and not cause trouble
        if (file.isDirectory && (file.isEmpty || file.name == "__MACOSX")) {
          logger.info(s"[${ draftDeposit.id }] cleaning up $file")
          file.delete()
          if (file.parent != targetDir)
            cleanup(file.parent)
        }
      }

      def buildMessage(cause: String) = {
        val printableCause = cause.replaceAll(printableRegexp, "?")
        draftDeposit.mailToDansMessage(
          ref = draftDeposit.id.toString,
          linkIntro = printableCause,
          bodyMsg =
            s"""Something went wrong while extracting file(s) from $uploadName to $path.
               |Cause: $printableCause
               |Could you please investigate this issue
               |""".
              stripMargin,
        )
      }

      def extractAll(firstEntry: ArchiveEntry) = {
        logger.info(s"[${ draftDeposit.id }] Extracting archive to $targetDir")
        for {
          _ <- extractOne(firstEntry)
          _ <- Stream
            .continually(archiveInputStream.getNextEntry)
            .takeWhile(Option(_).nonEmpty)
            .map(extractOne)
            .failFastOr(Success(()))
          _ = targetDir.walk().foreach(cleanup)
        } yield ()
      }

      lazy val noEntriesMsg = s"No entries found."

      Try(Option(archiveInputStream.getNextEntry))
        .recoverWith { case _: EOFException => Failure(MalformedArchiveException(uploadName, "file(s)", path, noEntriesMsg)) }
        .flatMap {
          case None => Failure(MalformedArchiveException(uploadName, "file(s)", path, escape(noEntriesMsg)))
          case Some(firstEntry: ArchiveEntry) => extractAll(firstEntry)
        }.recoverWith {
        case e: ZipException =>
          // for example the tested: Unexpected record signature: 0X88B1F
          Failure(MalformedArchiveException(uploadName, "file(s)", path, escape(e.getMessage)))
        case e: IOException if e.getCause != null && e.getCause.isInstanceOf[IllegalArgumentException] =>
          // for example EASY-2619: At offset ..., ... byte binary number exceeds maximum signed long value
          Failure(MalformedArchiveException(uploadName, "file(s)", path, buildMessage(e.getCause.getMessage)))
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
        managed(new TarArchiveInputStream(fileItem.getInputStream, TarConstants.DEFAULT_BLKSIZE, TarConstants.DEFAULT_RCDSIZE, charSet, true))
      else
        managed(new ZipArchiveInputStream(fileItem.getInputStream, charSet, true, true))
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
