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
  private lazy val uploadRoot = {
    lazy val children = bag.data.children.toList
    val original = bag.data / "original"
    if (!bag.data.exists || children.isEmpty || (children == List(original) && original.isDirectory))
      original
    else bag.data // a deposit created before uploading to 'original' was deployed
  }

  // though currently we don't create fetch files, let us not run into trouble whenever we do
  private lazy val fetchFiles = bag.fetchFiles
    .map(fetchFile =>
      bag.data.relativize(fetchFile.file)
    )

  private def cleanUp(bagPayloadPath: Path): Try[Any] = {
    val absFile = bag.data / bagPayloadPath.toString
    if (absFile.exists) {
      logger.info(s"removing payload file $absFile to be replaced by the newly uploaded file")
      bag.removePayloadFile(bagPayloadPath)
    }
    else if (fetchFiles.contains(bagPayloadPath)) {
      logger.info(s"removing fetch file $absFile to be replaced by the newly uploaded file")
      bag.removeFetchItem(bagPayloadPath)
    }
    else Success(())
  }

  /**
   * Moves files from stagingDir to draftBag, after deleting each file in the bag as soon as it would be overwritten.
   *
   * @param stagingDir                the temporary container for files, unique per request, same mount as draftBag
   * @param depositPayloadDestination path relative to the upload root that receives the staged files
   * @return
   */
  def moveAll(stagingDir: File, depositPayloadDestination: Path): Try[Unit] = {
    stagingDir
      .walk()
      .withFilter(!_.isDirectory)
      .map { stagedFile =>
        val relativeStagedPath = stagingDir.relativize(stagedFile)
        val depositPayloadPath = depositPayloadDestination.resolve(relativeStagedPath)
        val absDestinationFile = uploadRoot / depositPayloadPath.toString
        val bagPayloadPath = bag.data.relativize(absDestinationFile)
        for {
          _ <- cleanUp(bagPayloadPath)
          _ = logger.info(s"moving uploaded file $stagedFile to $bagPayloadPath of ${ bag.data }")
          _ <- bag.addPayloadFile(stagedFile, bagPayloadPath)(ATOMIC_MOVE)
        } yield ()
      }.failFastOr(bag.save)
  }

  /**
   * Returns 'true' if the path points to a directory.
   *
   * @param depositPayloadPath a relative path for the deposit.
   * @return 'true' if directory, else 'false'
   */
  def isDirectory(depositPayloadPath: Path): Boolean = {
    depositPayloadPath.toString.matches("/?") || (uploadRoot / depositPayloadPath.toString).isDirectory
  }

  /**
   * Lists information about the files in the directory `path` and its subdirectories.
   * A previous crash during a recursive delete process (deleting a directory),
   * or between deleting a single file and updating the manifest,
   * will result in showing deleted files.
   *
   * @param uploadRelativePath relative to the upload root of the bag.
   * @return a list of FileInfo objects
   */
  def list(uploadRelativePath: Path = Paths.get("")): Try[Seq[FileInfo]] = {
    val parentPath = uploadRoot / uploadRelativePath.toString

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

  private def toFileInfo(absFile: File, checksum: String): FileInfo = {
    FileInfo(absFile.name, uploadRoot.relativize(absFile.parent), checksum)
  }

  /**
   * Lists information about a file.
   *
   * @param depositRelativePath a relative path to the upload root.
   * @return a FileInfo object
   */
  def get(depositRelativePath: Path = Paths.get("")): Try[FileInfo] = {
    val absFile = uploadRoot / depositRelativePath.toString
    payloadManifest.map {
      fileToString =>
        fileToString
          .find(_._1 == absFile)
          .map((toFileInfo _).tupled)
          .map(Success(_))
          .getOrElse(Failure(NoSuchFileInDepositException(depositRelativePath)))
    } getOrElse Failure(new Exception(s"no manifest for ${ bag.baseDir }"))
  }

  private def payloadManifest: Option[Map[File, String]] = {
    val manifests = bag.payloadManifests
    manifests.get(SHA1).orElse(manifests.values.headOption)
  }

  /**
   * Write the input stream `is` to the relative path into the data files directory.
   *
   * @param is                  the input stream
   * @param depositRelativePath relative to the upload root
   * @return `true` if a new file was created, `false` if an existing file was overwritten
   */
  def write(is: InputStream, depositRelativePath: Path): Try[Boolean] = {
    val absFile = uploadRoot / depositRelativePath.toString
    val absFileExists = absFile.exists
    val bagPayloadPath = bag.data.relativize(absFile)
    for {
      _ <- if (absFileExists) cleanUp(bagPayloadPath)
           else Success(())
      _ <- bag.addPayloadFile(is, bagPayloadPath)
      _ <- bag.save
    } yield !absFileExists
  }

  /**
   * Deletes the file or directory located at the relative path into the upload root.
   * Directories are deleted recursively.
   *
   * @param depositPayloadPath the relative path of the file or directory to delete
   */
  def delete(depositPayloadPath: Path): Try[Unit] = {
    val absFile = uploadRoot / depositPayloadPath.toString
    if (!absFile.exists) {
      Failure(NoSuchFileInDepositException(depositPayloadPath))
    }
    else {
      val triedBag = if (absFile.isDirectory) removeDir(absFile.listRecursively().toStream)
                     else bag.removePayloadFile(bag.data.relativize(absFile))
      triedBag.flatMap(_.save)
    }
  }

  private def removeDir(absFiles: Stream[File]): Try[DansBag] = {
    absFiles
      .withFilter(!_.isDirectory)
      .map(f => bag.removePayloadFile(bag.data.relativize(f)))
      .failFastOr(Success(bag))
  }
}
