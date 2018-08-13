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

import better.files._
import nl.knaw.dans.bag.ChecksumAlgorithm.SHA1
import nl.knaw.dans.bag.DansBag
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
   *
   * @param path a relative path into `dataFilesBase`
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

  def toFileInfo(file: File, checksum: String): FileInfo = {
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

  /**
   * Deletes the file or directory located at the relative path into the data files directory. Directories
   * are deleted recursively.
   *
   * @param path the relative path of the file or directory to delete
   */
  def delete(path: Path): Try[Unit] = {
    val file = bag.data / path.toString
    if (!file.exists) Failure(new NoSuchFileException(path.toString))
    else if (file.isDirectory) removeDir(file.walk().toStream)
    else removeFile(path)
  }

  private def removeDir(files: Stream[File]) = {
    files
      .withFilter(!_.isDirectory)
      .map(f => removeFile(bag.data.relativize(f)))
      .find(_.isFailure)
      .getOrElse(Success(()))
  }

  private def removeFile(path: Path) = {
    // saving each mutation keeps the bag consistent in case of failures further down the chain
    bag
      .removePayloadFile(path)
      .flatMap(_.save)
  }
}
