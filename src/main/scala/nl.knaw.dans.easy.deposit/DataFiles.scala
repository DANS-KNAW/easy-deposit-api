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
import java.nio.file.{ NoSuchFileException, Path, Paths }
import java.util.zip.{ ZipEntry, ZipException, ZipInputStream }

import better.files._
import nl.knaw.dans.bag.ChecksumAlgorithm.SHA1
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.easy.deposit.servlets.DepositServlet.BadRequestException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

/**
 * Represents the data files of a deposit. The data files are the content files that the user uploads,
 * i.e. the files that are the actual target of preservation. The dataset metadata is ''not'' included
 * in this.
 *
 * @param bag the bag containing the data files
 */
case class DataFiles(bag: DansBag) extends DebugEnhancedLogging {

  /**
   * Lists information about the files the directory `path` and its subdirectories.
   * A previous crash during a recursive delete process (deleting a directory),
   * or between deleting a single file and updating the manifest,
   * will result in showing deleted files.
   *
   * @param path a relative path into data files directory of the bag.
   * @return a list of [[FileInfo]] objects
   */
  def list(path: Path = Paths.get("")): Try[Seq[FileInfo]] = {
    val parentPath = bag.baseDir / "data" / path.toString
    val manifestMap = bag.payloadManifests

    def convert(items: Map[File, String]) = {
      items
        .withFilter(_._1.isChildOf(parentPath))
        .map((toFileInfo _).tupled)
        .toSeq
    }

    manifestMap
      .get(SHA1)
      .orElse(manifestMap.values.headOption)
      .map(items => Success(convert(items)))
      .getOrElse(Failure(new Exception(s"no algorithm for ${ bag.baseDir }")))
  }

  private def toFileInfo(file: File, checksum: String): FileInfo = {
    FileInfo(file.name, bag.data.relativize(file.parent), checksum)
  }

  /**
   * Write the input stream `is` to the relative path into the data files directory.
   *
   * @param is   the input stream
   * @param path the relative path to the file to write
   * @return `true` if a new file was created, `false` if an existing file was overwritten
   */
  def write(is: InputStream, path: Path): Try[Boolean] = {
    val fileExists = (bag.data / path.toString).exists
    for {
      _ <- if (fileExists) removeFile(path)
           else Success(())
      _ <- bag.addPayloadFile(is, path)
      _ <- bag.save
    } yield !fileExists
  }

  def unzip(zipInputStream: ZipInputStream, path: Path): Try[Unit] = {
    def handleZipEntry(zipEntry: ZipEntry) = {
      if (zipEntry.isDirectory) Success(false)
      else {
        logger.info(s"extracting ${ zipEntry.getName } size=${ zipEntry.getSize } crc=${ zipEntry.getCrc } method=${ zipEntry.getMethod }")
        val fullPath = path.resolve(zipEntry.getName)
        write(zipInputStream, fullPath)
      }
    }

    while ( // TODO https://github.com/DANS-KNAW/easy-deposit-api/pull/65#discussion_r213627555
      Try(zipInputStream.getNextEntry)
        .map(Option(_).map(handleZipEntry)) match {
        case Success(None) => false // end of zip
        case Success(Some(Success(_))) => true // extracted and uploaded
        case Success(Some(Failure(e))) => return Failure(e) // could not save
        case Failure(e) if e.isInstanceOf[ZipException] => // could not extract TODO still fail fast? Other files might have been uploaded.
          return Failure(BadRequestException(s"ZIP file is malformed.  $e"))
        case Failure(e) => return Failure(e)
      }) {}
    Success(())
  }

  /**
   * Deletes the file or directory located at the relative path into the data files directory. Directories
   * are deleted recursively.
   *
   * @param path the relative path of the file or directory to delete
   */
  def delete(path: Path): Try[Unit] = {
    val file = bag.data / path.toString
    if (!file.exists) Failure(new NoSuchFileException(path.toString))
    else (if (file.isDirectory) removeDir(file.walk().toStream)
          else removeFile(path)
      ).flatMap(_.save)
  }

  private def removeDir(files: Stream[File]): Try[DansBag] = {
    files
      .withFilter(!_.isDirectory)
      .map(f => removeFile(bag.data.relativize(f)))
      .find(_.isFailure) // fail fast
      .getOrElse(Success(bag))
  }

  private def removeFile(path: Path): Try[DansBag] = {
    // saving after each file causes calculation of at least 4 check sums in tagmanifest
    // still one deleted file can show up after a crash between delete and completing the save
    bag.removePayloadFile(path)
  }
}
