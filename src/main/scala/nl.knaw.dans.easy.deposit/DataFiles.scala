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
import nl.knaw.dans.bag.ImportOption.ATOMIC_MOVE
import nl.knaw.dans.easy.deposit.Errors.NoSuchFileInDepositException
import nl.knaw.dans.easy.deposit.docs.FileInfo
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
  private val uploadRoot = bag.data / "original"

  // though currently we don't create fetch files, let us not run into trouble whenever we do
  private lazy val fetchFiles = bag.fetchFiles
    .map(fetchFile =>
      bag.data.relativize(fetchFile.file)
    )

  def cleanUp(bagRelativePath: Path): Try[Any] = {
    val oldFile = bag.data / bagRelativePath.toString
    if (oldFile.exists) {
      logger.info(s"removing payload file $bagRelativePath to be replaced by the newly uploaded file") // TODO prefix?
      bag.removePayloadFile(bagRelativePath)
    }
    else if (fetchFiles.contains(bagRelativePath)) {
      logger.info(s"removing fetch file $bagRelativePath to be replaced by the newly uploaded file")
      bag.removeFetchItem(bagRelativePath)
    }
    else Success(())
  }

  /**
   * Moves files from stagingDir to draftBag, after deleting each file in the bag as soon as it would be overwritten.
   *
   * @param stagingDir      the temporary container for files, unique per request, same mount as draftBag
   * @param destinationRoot path relative to the upload root that receives the staged files
   * @return
   */
  def moveAll(stagingDir: File, destinationRoot: Path): Try[Unit] = {
    stagingDir
      .walk()
      .withFilter(!_.isDirectory)
      .map { stagedFile =>
        val relativeStagedPath = stagingDir.relativize(stagedFile)
        val destination = destinationRoot.resolve(relativeStagedPath)
        val bagAbsFile = uploadRoot / destination.toString
        val bagRelativePath = bag.data.relativize(bagAbsFile)
        for {
          _ <- cleanUp(bagRelativePath)
          _ = logger.info(s"moving uploaded file $stagedFile to $bagRelativePath of ${ bag.data }")
          _ <- bag.addPayloadFile(stagedFile, bagRelativePath)(ATOMIC_MOVE)
        } yield ()
      }.failFastOr(bag.save)
  }

  /**
   * Returns 'true' if the path points to a directory.
   *
   * @param path a relative path.
   * @return 'true' if directory, else 'false'
   */
  def isDirectory(path: Path): Boolean = {
    // TODO so far this is the only method with backward compatibility (determines whether list or get is called)
    (uploadRoot / path.toString).isDirectory || (bag.data / path.toString).isDirectory
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
    val parentPath = uploadRoot / path.toString

    def convert(items: Map[File, String]) = {
      items
        .withFilter(_._1.isChildOf(parentPath))
        .map((toFileInfo _).tupled)
        .toSeq
        .sortBy(fileInfo => (File(fileInfo.dirpath) / fileInfo.filename).path)
    }

    payloadManifest
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
    val requestedFile = uploadRoot / path.toString
    payloadManifest.map {
      fileToString =>
        fileToString
          .find(_._1 == requestedFile)
          .map((toFileInfo _).tupled)
          .map(Success(_))
          .getOrElse(Failure(NoSuchFileInDepositException(path)))
    } getOrElse Failure(new Exception(s"no manifest for ${ bag.baseDir }"))
  }

  private def payloadManifest = {
    val manifests = bag.payloadManifests
    manifests.get(SHA1).orElse(manifests.values.headOption)
  }

  /**
   * Write the input stream `is` to the relative path into the data files directory.
   *
   * @param is   the input stream
   * @param path the relative path to the file to write
   * @return `true` if a new file was created, `false` if an existing file was overwritten
   */
  def write(is: InputStream, path: Path): Try[Boolean] = {
    val fileExists = (uploadRoot / path.toString).exists
    val pathInDataDir = bag.data.relativize(uploadRoot / path.toString)
    for {
      _ <- if (fileExists) cleanUp(pathInDataDir) // TODO see fetchFiles in StagedFilesTarget
           else Success(())
      _ <- bag.addPayloadFile(is, pathInDataDir)
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
    val file = uploadRoot / path.toString
    if (!file.exists) {
      Failure(NoSuchFileInDepositException(path))
    }
    else {
      val triedBag = if (file.isDirectory) removeDir(file.walk().toStream)
                     else bag.removePayloadFile(bag.data.relativize(file))
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
