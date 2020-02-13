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
import java.nio.file.{ Path, Paths }

import better.files._
import nl.knaw.dans.bag.ChecksumAlgorithm.SHA1
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.easy.deposit.Errors.NoSuchFileInDepositException
import nl.knaw.dans.easy.deposit.docs.FileInfo
import nl.knaw.dans.easy.deposit.servlets.DepositServlet
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }

/**
 * Represents the data files of a deposit. The data files are the content files that the user uploads,
 * i.e. the files that are the actual target of preservation. The dataset metadata is ''not'' included
 * in this.
 *
 * @param bag the bag containing the data files
 */
case class DataFiles(bag: DansBag) extends DebugEnhancedLogging {
  private val uploadRoot = bag.data / DepositServlet.uploadRoot

  /**
   * Returns 'true' if the path points to a directory.
   *
   * @param path a relative path.
   * @return 'true' if directory, else 'false'
   */
  def isDirectory(path: Path): Boolean = {
    val file = bag.data / path.toString
    file == uploadRoot || file.isDirectory
  }

  /**
   * Lists information about the files in the directory `path` and its subdirectories.
   * A previous crash during a recursive delete process (deleting a directory),
   * or between deleting a single file and updating the manifest,
   * will result in showing deleted files.
   *
   * @param path a relative path into data files directory of the bag.
   * @return a list of FileInfo objects
   */
  def list(path: Path = Paths.get("")): Try[Seq[FileInfo]] = {
    val parentPath = bag.data / path.toString
    val manifestMap = bag.payloadManifests

    def convert(items: Map[File, String]) = {
      items
        .withFilter(_._1.isChildOf(parentPath))
        .map((toFileInfo _).tupled)
        .toSeq
        .sortBy(fileInfo => (File(fileInfo.dirpath) / fileInfo.filename).path)
    }

    manifestMap
      .get(SHA1)
      .orElse(manifestMap.values.headOption)
      .map(items => Success(convert(items)))
      .getOrElse(Failure(new Exception(s"no algorithm for ${ bag.baseDir }")))
  }

  private def toFileInfo(file: File, checksum: String): FileInfo = {
    FileInfo(file.name, uploadRoot.relativize(file.parent), checksum)
  }

  /**
   * Lists information about a file.
   *
   * @param path a relative path into a data files.
   * @return a FileInfo object
   */
  def get(path: Path = Paths.get("")): Try[FileInfo] = {
    val manifestMap = bag.payloadManifests
    val manifests = manifestMap.get(SHA1).orElse(manifestMap.values.headOption)
    val absolutePath = bag.data / path.toString
    val fileExists = manifests.get.contains(absolutePath)
    if (fileExists) Success(toFileInfo(absolutePath, checksum = manifests get absolutePath))
    else {
      val relativePath = uploadRoot.relativize(absolutePath)
      Failure(NoSuchFileInDepositException(relativePath))
    }
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
      _ <- if (fileExists) bag.removePayloadFile(path)
           else Success(())
      _ <- bag.addPayloadFile(is, path)
      _ <- bag.save
    } yield !fileExists
  }

  /**
   * Deletes the file or directory located at the relative path into the data files directory. Directories
   * are deleted recursively.
   *
   * @param path the relative path of the file or directory to delete
   */
  def delete(path: Path): Try[Unit] = {
    val file = bag.data / path.toString
    if (!file.exists) {
      val relPath = uploadRoot.relativize(file)
      Failure(NoSuchFileInDepositException(relPath))
    } else {
      val triedBag = if (file.isDirectory) removeDir(file.walk().toStream)
                     else bag.removePayloadFile(path)
      triedBag.flatMap(_.save)
    }
  }

  private def removeDir(files: Stream[File]): Try[DansBag] = {
    files
      .withFilter(!_.isDirectory)
      .map(f => bag.removePayloadFile(bag.data.relativize(f)))
      .failFastOr(Success(bag))
  }
}
