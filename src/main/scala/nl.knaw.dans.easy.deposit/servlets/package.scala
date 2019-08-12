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
import java.nio.file.Files
import java.util.UUID
import java.util.zip.{ ZipEntry, ZipException }

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.deposit.Errors.{ ConfigurationException, MalformedZipException, ZipMustBeOnlyFileException }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.compress.archivers.ArchiveEntry
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.scalatra.servlet.{ FileItem, MultipartConfig }
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

  implicit class RichManagedZipInputStream(val zipInputStream: ManagedResource[ZipArchiveInputStream]) extends AnyVal {
    def unzipPlainEntriesTo(dir: File): Try[Unit] = {
      zipInputStream.apply(_.unzipPlainEntriesTo(dir))
    }
  }

  implicit class RichZipInputStream(val zipInputStream: ZipArchiveInputStream) extends AnyVal {

    private def skip(file: File): Boolean = {
      file.isDirectory && file.name == "__MACOSX"
    }

    def unzipPlainEntriesTo(dir: File): Try[Unit] = {
      def extract(entry: ArchiveEntry): Try[Unit] = {
        if (entry.isDirectory)
          Try((dir / entry.getName).createDirectories())
        else {
          logger.info(s"Extracting ${ entry.getName } size=${ entry.getSize } getLastModifiedDate=${ entry.getLastModifiedDate } }")
          Try {
            (dir / entry.getName).parent.createDirectories() // in case a directory was not specified separately
            Files.copy(zipInputStream, (dir / entry.getName).path)
            ()
          }.recoverWith { case e: ZipException =>
            logger.error(e.getMessage, e)
            Failure(MalformedZipException(s"Can't extract ${ entry.getName }"))
          }
        }
      }

      Try(Option(zipInputStream.getNextEntry)) match {
        case Success(None) |
             Failure(_: EOFException) => Failure(MalformedZipException(s"No entries found."))
        case Failure(e: ZipException) => Failure(MalformedZipException(e.getMessage))
        case Failure(e) => Failure(e)
        case Success(Some(firstEntry: ArchiveEntry)) => for {
          _ <- extract(firstEntry)
          _ <- Stream
            .continually(zipInputStream.getNextEntry)
            .takeWhile(Option(_).nonEmpty)
            .map(extract)
            .failFastOr(Success(()))
          _ = dir.list(skip).foreach(_.delete())
        } yield ()
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

    def getZipInputStream: Try[resource.ManagedResource[ZipArchiveInputStream]] = Try {
      fileItem.charset
        .map(toZipInputStream)
        .getOrElse(toZipInputStream("UTF8"))
    }

    private def toZipInputStream(charSet: String): ManagedResource[ZipArchiveInputStream] = {
      val useUnicodeExtraFields = true
      val allowStoredEntriesWithDataDescriptor = true
      managed(new ZipArchiveInputStream(fileItem.getInputStream, charSet, useUnicodeExtraFields, allowStoredEntriesWithDataDescriptor))
    }
  }

  implicit class RichMultipartConfig(config: MultipartConfig) {
    def moveNonZips(srcItems: Iterator[FileItem], targetDir: File): Try[Unit] = {
      srcItems
        .map(moveIfNonZip(_, targetDir))
        .failFastOr(Success(()))
    }

    private def moveIfNonZip(srcItem: FileItem, targetDir: File): Try[Unit] = {
      logger.info(s"staging upload: size=${ srcItem.size } contentType=${ srcItem.contentType } $targetDir/${ srcItem.name }")
      if (srcItem.name.isBlank) Success(()) // skip form field without selected files
      else if (srcItem.isZip) Failure(ZipMustBeOnlyFileException(srcItem))
      else Try {
        val f = UUID.randomUUID().toString
        val location = File(config.location.getOrElse(throw ConfigurationException("multipart.location is missing")))

        // Try to move the (big) uploaded file to a known name,
        // otherwise write the in-memory content, depending on the MultipartConfig values of the servlet.
        srcItem.part.write(f)

        // now we can move the upload to the location we really want
        (location / f).moveTo(targetDir / srcItem.name)(CopyOptions.atomically)
      }
    }
  }

  implicit class RichFileItems(val fileItems: BufferedIterator[FileItem]) extends AnyVal {

    def nextAsZipIfOnlyOne: Try[Option[ManagedResource[ZipArchiveInputStream]]] = {
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
