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

import scala.util.Try

/**
 * Represents the data files of a deposit. The data files are the content files that the user uploads,
 * i.e. the files that are the actual target of preservation. The dataset metadata is ''not'' included
 * in this.
 *
 * @param dataFilesBase the base directory of the data files
 * @param filesMetaData the file containing the file metadata
 */
case class DataFiles(dataFilesBase: File, filesMetaData: File) {

  /**
   * Lists information about the files the directory `path` and its subdirectories.
   *
   * @param path a relative path into `dataFilesBase`
   * @return a list of [[FileInfo]] objects
   */
  def list(path: Path = Paths.get("")): Try[Seq[FileInfo]] = ???

  /**
   * Write the inputstream `is` to the relative path into the data files directory.
   *
   * @param is   the input stream
   * @param path the relative path to the file to write
   * @return `true` if a new file was created, `false` if an existing file was overwritten
   */
  def write(is: InputStream, path: Path): Try[Boolean] = Try {
    val file: File = dataFilesBase / path.toString
    val createFile = !file.exists
    file.createIfNotExists(asDirectory = false, createParents = true)
    file.outputStream.foreach(is.pipeTo(_))
    createFile
  }

  /**
   * Deletes the file or directory located at the relative path into the data files directory. Directories
   * are deleted recursively.
   *
   * @param path the relative path of the file or directory to delete
   */
  def delete(path: Path): Try[Unit] = Try {
    // TODO when a sha was saved at upload, delete it
    (dataFilesBase / path.toString).delete()
  }

  /**
   * Write `files.xml`
   */
  def writeFileMetadata(): Try[Unit] = ???
}
