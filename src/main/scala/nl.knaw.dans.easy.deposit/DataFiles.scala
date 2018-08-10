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
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

/**
 * Represents the data files of a deposit. The data files are the content files that the user uploads,
 * i.e. the files that are the actual target of preservation. The dataset metadata is ''not'' included
 * in this.
 *
 * @param dataFilesBase the base directory of the data files
 */
case class DataFiles(dataFilesBase: File) extends DebugEnhancedLogging {

  /**
   * Lists information about the files the directory `path` and its subdirectories.
   *
   * @param path a relative path into `dataFilesBase`
   * @return a list of [[FileInfo]] objects
   */
  def list(path: Path = Paths.get("")): Try[Seq[FileInfo]] = {
    getManifest.map { case (_, manifest) => // TODO reads all manifests and metadata, need just one manifest
      manifest.withFilter { case (file, _) =>
        path.toString == "" || dataFilesBase.relativize(file).startsWith(path)
      }.map { case (file, checksum) =>
        FileInfo(file.name, dataFilesBase.relativize(file.parent), checksum)
      }.toSeq
    }
  }

  /**
   * Write the inputstream `is` to the relative path into the data files directory.
   *
   * @param is   the input stream
   * @param path the relative path to the file to write
   * @return `true` if a new file was created, `false` if an existing file was overwritten
   */
  def write(is: InputStream, path: Path): Try[Boolean] = {
    for {
      (bag, manifest) <- getManifest
      fileExists = manifest.exists { case (p, _) => dataFilesBase.relativize(p) == path }
      _ <- if (fileExists) bag.removePayloadFile(path)
           else Success(())
      _ <- if (fileExists) bag.save
           else Success(())
      _ <- bag.addPayloadFile(is, path)
      _ <- bag.save
    } yield !fileExists
  }

  private def getManifest = for {
    bag <- DansV0Bag.read(dataFilesBase.parent)
    alg <- Try { bag.payloadManifestAlgorithms.headOption.get }
    manifest <- Try { bag.payloadManifests(alg) }
  } yield (bag, manifest)

  /**
   * Deletes the file or directory located at the relative path into the data files directory. Directories
   * are deleted recursively.
   *
   * @param path the relative path of the file or directory to delete
   */
  def delete(path: Path): Try[Unit] = {
    val file = dataFilesBase / path.toString
    for {
      _ <- if (file.exists) Success(())
           else Failure(new NoSuchFileException(path.toString))
      bag <- DansV0Bag.read(dataFilesBase.parent)
      _ <- if (file.isDirectory) removeDir(bag, file.walk().toStream)
           else bag.removePayloadFile(path)
      _ <- bag.save
    } yield ()
  }

  private def removeDir(bag: DansV0Bag, files: Stream[File]) = {
    def remove(file: File) = {
      bag.removePayloadFile(dataFilesBase.relativize(file))
    }

    files.withFilter(!_.isDirectory).map(remove).find(_.isFailure).getOrElse(Success(()))
  }
}
