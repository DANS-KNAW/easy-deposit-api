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

import java.io.InputStream
import java.nio.charset.Charset
import java.util.zip.ZipInputStream

import better.files.ManagedResource
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.ZipMustBeOnlyFileException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatra.servlet.FileItem
import org.scalatra.util.RicherString._
import org.scalatra.{ ActionResult, BadRequest, InternalServerError }
import resource.managed

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

  implicit class RichIterator[T](val xs: Iterator[Try[T]]) extends AnyVal {
    def failFast: Try[Seq[T]] = {
      // TODO dans-lib candidate?
      val successes = Seq.newBuilder[T]

      xs.foreach {
        case Success(t) => successes += t
        case Failure(e) =>
          return Failure(e)
      }

      Success(successes.result())
    }
  }

  implicit class RichOptionTry[T](val ot: Option[Try[T]]) extends AnyVal {
    // TODO candidate for dans-lib?
    def insideOut: Try[Option[T]] = ot match {
      case Some(Success(t)) => Success(Some(t))
      case Some(Failure(e)) => Failure(e)
      case None => Success(None)
    }
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
  }

  implicit class RichFileItems(val fileItems: BufferedIterator[FileItem]) extends AnyVal {

    def nextIfOnlyZip: Try[Option[FileItem]] = {

      // forward pointer after checking for leading form fields without selected files
      while (fileItems.head.name.isBlank) { fileItems.next() }

      if (!fileItems.head.isZip) Success(None)
      else {
        val zipItem = fileItems.next()
        if (fileItems.hasNext)
          Failure(ZipMustBeOnlyFileException(zipItem.name))
        else Success(Some(zipItem))
      }
    }
  }
}
