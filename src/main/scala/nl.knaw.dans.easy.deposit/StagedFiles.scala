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
import java.nio.file.{ Files, Path }
import java.util.zip.{ ZipEntry, ZipException, ZipInputStream }

import better.files.File
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.BadRequestException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

/**
 *
 * @param stagingDir the temporary container for files, unique per request, same mount as bag
 * @param draftBag   the bag that receives the staged files
 * @param path       relative location in the bag's data directory
 */
case class StagedFiles(stagingDir: File, draftBag: DansBag, path: Path) extends DebugEnhancedLogging {

  private val target: File = draftBag.baseDir / path.toString
  logger.info(s"staging to $stagingDir for $target")

  private def moveToDraft(file: File): Try[DansBag] = {
    val fullPath = path.resolve(stagingDir.relativize(file))
    logger.debug(s"moving to $fullPath")
    draftBag.addPayloadFile(file, fullPath) // TODO move, not copy again
  }

  def moveAll: Try[Any] = {
    logger.info(s"moving from staging to $target")
    stagingDir
      .walk()
      .filter(!_.isDirectory)
      .map(moveToDraft)
      .find(_.isFailure)
      .getOrElse(Success(()))
  }

  def unzip(is: InputStream, charset: Option[String]): Try[Unit] = {
    Try(charset
      .map(charSet => new ZipInputStream(is, Charset.forName(charSet)))
      .getOrElse(new ZipInputStream(is))
    ).flatMap { zipInputStream =>

      def extract(entry: ZipEntry) = {
        if (entry.isDirectory)
          Try((stagingDir / entry.getName).createDirectories())
        else if (entry.getName.matches(".*.g?z(ip)?")) // TODO the regexp is duplicated from DepositServlet
               Failure(BadRequestException(s"ZIP file is malformed. It contains a Zip ${ entry.getName }."))
        else {
          logger.info(s"Extracting ${ entry.getName } size=${ entry.getSize } compressedSize=${ entry.getCompressedSize } CRC=${ entry.getCrc }")
          Try(Files.copy(zipInputStream, (stagingDir / entry.getName).path))
        }.recoverWith {
          case e if e.isInstanceOf[ZipException] => Failure(BadRequestException(s"ZIP file is malformed. $e"))
        }
      }

      Option(zipInputStream.getNextEntry) match {
        case None => Failure(BadRequestException(s"ZIP file is malformed. No entries found."))
        case Some(firstEntry: ZipEntry) => for {
          _ <- extract(firstEntry)
          _ <- Stream
            .continually(zipInputStream.getNextEntry)
            .takeWhile(Option(_).nonEmpty)
            .map(extract)
            .find(_.isFailure)
            .getOrElse(Success(()))
          _ <- moveAll
        } yield ()
      }
    }
  }
}
