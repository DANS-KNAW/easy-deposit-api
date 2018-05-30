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
import java.util.UUID

import scala.xml.Elem
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.AccessCategory.AccessCategory
import better.files._
import org.apache.tika.Tika

import scala.collection.Seq
import scala.util.{ Failure, Try }

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
   * Generates files.xml from files in draft bag
   *
   * @param datasetAccessCategory access type
   * @return Elem
   */
  def getFilesXml(datasetAccessCategory: AccessCategory, pathData: Path): Try[Elem] = Try {
    val files = dataFilesBase.walk()
    val tika = new Tika

    val fileXML: File = dataFilesBase / "metadata/files.xml"
    val createFile = !fileXML.exists
    fileXML.createIfNotExists(asDirectory = false, createParents = true)
    fileXML.append("<?xml version='1.0' encoding='UTF-8'?>")
    fileXML.append("files xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns=\"http://easy.dans.knaw.nl/schemas/bag/metadata/files/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n       xsi:schemaLocation=\"http://purl.org/dc/terms/ http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd http://easy.dans.knaw.nl/schemas/bag/metadata/files/ http://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd\"")



    val file = dataFilesBase / "data"
    if (!file.exists)
      Try{Seq.empty}
    else
      Try {
        file
          .list
          .filter(!_.isDirectory)
          .map(
            x => new DepositDir(baseDir, user, UUID.fromString(x.name))
          )
          .toSeq
      }





//      <file filepath="data/path/to/a/random/video/hubble.mpg">
//        <dcterms:format>tika.detect(path)</dcterms:format>
//    fileXML.append("</file>")


    fileXML.append("</file>")


    ???
  }


  /**
   * Deletes the file or directory located at the relative path into the data files directory. Directories
   * are deleted recursively.
   *
   * @param path the relative path of the file or directory to delete
   */
  def delete(path: Path): Try[Unit] = ???

  def writeFilesXml(): Try[Unit] = ???
}

object AddFileMetadataToDeposit {
  private val tika = new Tika
  type MimeType = String

  /**
   * Identify the mimeType of a path.
   *
   * @param path the path to identify
   * @return the mimeType of the path if the identification was successful; `Failure` otherwise
   */
  def getMimeType(path: Path): Try[MimeType] = Try {
    tika.detect(path)
  }
}
