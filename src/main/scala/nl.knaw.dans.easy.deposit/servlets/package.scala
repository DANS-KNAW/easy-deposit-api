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

import java.io.EOFException
import java.nio.charset.Charset
import java.nio.file.Files
import java.util.zip.{ ZipEntry, ZipException, ZipInputStream }

import better.files.File
import nl.knaw.dans.easy.deposit.Errors.{ MalformedZipException, ZipMustBeOnlyFileException }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.servlet.FileItem
import org.scalatra.util.RicherString._
import resource.{ ManagedResource, managed }

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

  val extensionZipPattern = ".+[.]g?z(ip)?"
  private val pre = "(x-)?g?zip"
  private val post = "-compress(ed)?"
  val contentTypeZipPattern = s"application/(($pre($post)?)|(x$post))"

  val contentTypeJson: (String, String) = "content-type" -> "application/json;charset=UTF-8"
  val contentTypePlainText: (String, String) = "content-type" -> "text/plain;charset=UTF-8"

  implicit class RichManagedZipInputStream(val zipInputStream: ManagedResource[ZipInputStream]) extends AnyVal {
    def unzipPlainEntriesTo(dir: File): Try[Unit] = {
      zipInputStream.apply(_.unzipPlainEntriesTo(dir))
    }
  }

  implicit class RichZipInputStream(val zipInputStream: ZipInputStream) extends AnyVal {
    def unzipPlainEntriesTo(dir: File): Try[Unit] = {
      def extract(entry: ZipEntry): Try[Unit] = {
        if (entry.isDirectory)
          Try((dir / entry.getName).createDirectories())
        else {
          logger.info(s"Extracting ${ entry.getName } size=${ entry.getSize } compressedSize=${ entry.getCompressedSize } CRC=${ entry.getCrc }")
          Try { Files.copy(zipInputStream, (dir / entry.getName).path); () }
            .recoverWith { case e: ZipException =>
              logger.error(e.getMessage, e)
              Failure(MalformedZipException(s"Can't extract ${ entry.getName }"))
            }
        }
      }

      Try(Option(zipInputStream.getNextEntry)) match {
        case Success(Some(firstEntry: ZipEntry)) => for {
          _ <- extract(firstEntry)
          _ <- Stream
            .continually(zipInputStream.getNextEntry)
            .takeWhile(Option(_).nonEmpty)
            .map(extract)
            .failFastOr(Success(()))
        } yield ()
        case Success(None) => Failure(MalformedZipException(s"No entries found."))
        case Failure(_: EOFException) => Failure(MalformedZipException(s"No entries found."))
        case Failure(e) => Failure(e)
      }
    }
  }

  implicit class RichFileItem(val fileItem: FileItem) extends AnyVal {

    def isZip: Boolean = {
      val extensionIsZip = fileItem.name.matches(extensionZipPattern)
      lazy val contentTypeIsZip = fileItem.contentType.exists(_.matches(contentTypeZipPattern))
      logger.debug(s"ZIP check: ${ fileItem.name } : $extensionIsZip; ${ fileItem.contentType } : $contentTypeIsZip ")
      extensionIsZip || contentTypeIsZip
    }

    def getZipInputStream: Try[resource.ManagedResource[ZipInputStream]] = Try {
      fileItem.charset
        .map(charSet => managed(new ZipInputStream(fileItem.getInputStream, Charset.forName(charSet))))
        .getOrElse(managed(new ZipInputStream(fileItem.getInputStream)))
    }

    def copyNonZipTo(dir: File): Try[Unit] = {
      if (fileItem.name.isBlank) Success(()) // skip form field without selected files
      else if (fileItem.isZip) Failure(ZipMustBeOnlyFileException(fileItem))
      else
        managed(fileItem.getInputStream)
          .apply(inputStream => Try { Files.copy(inputStream, (dir / fileItem.name).path) })
    }
  }

  implicit class RichFileItems(val fileItems: BufferedIterator[FileItem]) extends AnyVal {

    def copyPlainItemsTo(dir: File): Try[Unit] = {
      fileItems.toStream
        .map(_.copyNonZipTo(dir))
        .failFastOr(Success(()))
    }

    def nextAsZipIfOnlyOne: Try[Option[ManagedResource[ZipInputStream]]] = {
      skipLeadingEmptyFormFields()
      if (!fileItems.headOption.exists(_.isZip)) Success(None)
      else {
        val leadingZipItem = fileItems.next()
        skipLeadingEmptyFormFields()
        if (fileItems.hasNext)
          Failure(ZipMustBeOnlyFileException(leadingZipItem))
        else leadingZipItem.getZipInputStream.map(Some(_))
      }
    }

    private def skipLeadingEmptyFormFields(): Unit = {
      // forward pointer after check
      while (fileItems.headOption.exists(_.name.isBlank)) { fileItems.next() }
    }
  }
}
