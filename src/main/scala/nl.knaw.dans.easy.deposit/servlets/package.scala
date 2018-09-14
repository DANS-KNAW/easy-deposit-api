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

import java.nio.charset.Charset
import java.nio.file.Files
import java.util.zip.ZipInputStream

import better.files.File
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.ZipMustBeOnlyFileException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.servlet.FileItem
import org.scalatra.util.RicherString._
import org.scalatra.{ ActionResult, BadRequest, InternalServerError }
import resource.{ ManagedResource, managed }

import scala.util.{ Failure, Success, Try }

package object servlets extends DebugEnhancedLogging {

  def internalErrorResponse(t: Throwable): ActionResult = {
    logger.error(s"Not expected exception: ${ t.getMessage }", t)
    InternalServerError("Internal Server Error")
  }

  def badDocResponse(t: InvalidDocumentException): ActionResult = {
    logger.error(t.getMessage)
    BadRequest(s"Bad Request. ${ t.getMessage }")
  }

  implicit class RichFileItem(val fileItem: FileItem) extends AnyVal {

    def isZip: Boolean = {
      val extensionIsZip = fileItem.name.matches(".*.g?z(ip)?")
      lazy val contentTypeIsZip = fileItem.contentType.exists(_.matches(
        "(application|multipart)/(x-)?g?zip(-compress(ed)?)?( .*)?"
      ))
      logger.debug(s"ZIP check: ${ fileItem.name } : $extensionIsZip; ${ fileItem.contentType } : $contentTypeIsZip ")
      extensionIsZip || contentTypeIsZip
    }

    def getZipInputStream: Try[resource.ManagedResource[ZipInputStream]] = Try {
      fileItem.charset
        .map(charSet => managed(new ZipInputStream(fileItem.getInputStream, Charset.forName(charSet))))
        .getOrElse(managed(new ZipInputStream(fileItem.getInputStream)))
    }

    def ifNotZipCopyTo(dir: File): Try[Unit] = {
      if (fileItem.name.isBlank) Success(()) // skip form field without selected files
      else if (fileItem.isZip) Failure(ZipMustBeOnlyFileException(fileItem.name))
      else
        managed(fileItem.getInputStream)
          .apply(inputStream => Try { Files.copy(inputStream, (dir / fileItem.name).path) })
    }
  }

  implicit class RichFileItems(val fileItems: BufferedIterator[FileItem]) extends AnyVal {

    def copyPlainItemsTo (dir: File): Try[Unit] = {
      fileItems
        .map(_.ifNotZipCopyTo(dir))
        .find(_.isFailure)
        .getOrElse(Success(()))
    }

    def nextAsZipIfOnlyOne: Try[Option[ManagedResource[ZipInputStream]]] = {

      // forward pointer after checking for leading form fields without selected files
      while (fileItems.head.name.isBlank) { fileItems.next() }

      if (!fileItems.head.isZip) Success(None)
      else {
        val zipItem = fileItems.next() // TODO only empty form fields would be OK
        if (fileItems.hasNext)
          Failure(ZipMustBeOnlyFileException(zipItem.name))
        else zipItem.getZipInputStream.map(Some(_))
      }
    }
  }
}
