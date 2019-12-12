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

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.PosixFilePermission._
import java.nio.file.attribute.{ PosixFileAttributeView, UserPrincipalNotFoundException }
import java.util.UUID
import java.util.concurrent.ThreadPoolExecutor

import better.files.File
import better.files.File.{ CopyOptions, VisitOptions }
import nl.knaw.dans.bag.ChecksumAlgorithm.ChecksumAlgorithm
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.Errors.{ AlreadySubmittedException, InvalidDoiException }
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import org.joda.time.DateTime

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

/**
 * Object that contains the logic for submitting a deposit.
 *
 * @param stagingBaseDir  the base directory for staged copies
 * @param submitToBaseDir the directory to which the staged copy must be moved.
 */
class Submitter(stagingBaseDir: File,
                submitToBaseDir: File,
                groupName: String,
                depositUiURL: String,
                executor: ThreadPoolExecutor
               ) extends DebugEnhancedLogging {
  private val groupPrincipal = {
    Try {
      stagingBaseDir.fileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(groupName)
    }.getOrRecover {
      case e: UserPrincipalNotFoundException => throw new IOException(s"Group $groupName could not be found", e)
      case e: UnsupportedOperationException => throw new IOException("Not on a POSIX supported file system", e)
      case NonFatal(e) => throw new IOException(s"unexpected error occured on $stagingBaseDir", e)
    }
  }
  private val depositorInfoDirectoryName = "depositor-info"

  StartupValidation.allowsAtomicMove(srcDir = stagingBaseDir, targetDir = submitToBaseDir)

  /**
   * Submits `depositDir` by writing the file metadata, updating the bag checksums, staging a copy
   * and moving that copy to the submit-to area.
   *
   * @param draftDeposit the deposit object to submit
   * @return the UUID of the deposit in the submit area (easy-ingest-flow-inbox)
   */
  def submit(draftDeposit: DepositDir, stateManager: StateManager, user: UserInfo, stagedDir: File): Try[UUID] = {
    val propsFileName = "deposit.properties"
    for {
      // EASY-1464 step 3.3.4 validation
      //   [v] mandatory fields are present and not empty (by DatasetXml(datasetMetadata) in createXMLs)
      //   [v] DOI in json matches properties
      //   [ ] URLs are valid
      //   [ ] ...
      // EASY-1464 3.3.5.a: generate (with some implicit validation) content for metadata files
      draftBag <- draftDeposit.getDataFiles.map(_.bag)
      datasetMetadata <- draftDeposit.getDatasetMetadata
      agreementsXml <- AgreementsXml(DateTime.now, datasetMetadata, user)
      _ = datasetMetadata.doi.getOrElse(throw InvalidDoiException(draftDeposit.id))
      _ <- draftDeposit.sameDOIs(datasetMetadata)
      datasetXml <- DDM(datasetMetadata)
      msg4DataManager = {
        val firstPart = datasetMetadata.messageForDataManager.getOrElse("").stripLineEnd
        val secondPart = s"The deposit can be found at $depositUiURL/${ draftDeposit.id }"
        firstPart.toOption.fold(secondPart)(_ + "\n\n" + secondPart)
      }
      filesXml <- FilesXml(draftBag.data)
      _ <- sameFiles(draftBag.payloadManifests, draftBag.baseDir / "data")
      // from now on no more user errors but internal errors
      // EASY-1464 3.3.8.a create empty staged bag to take a copy of the deposit
      stageBag <- DansV0Bag.empty(stagedDir / bagDirName).map(_.withCreated())
      // EASY-1464 3.3.6 change state and copy with the rest of the deposit properties to staged dir
      _ <- stateManager.changeState(StateInfo(State.submitted, "The deposit is being processed"))
      submittedId <- stateManager.getSubmittedBagId // created by changeState
      submitDir = submitToBaseDir / submittedId.toString
      _ = if (submitDir.exists) throw AlreadySubmittedException(draftDeposit.id)
      _ = (draftBag.baseDir.parent / propsFileName).copyTo(stagedDir / propsFileName)
      // EASY-1464 3.3.5.b: write files to metadata
      _ = stageBag.addMetadataFile(msg4DataManager, s"$depositorInfoDirectoryName/message-from-depositor.txt")
      _ <- stageBag.addMetadataFile(agreementsXml, s"$depositorInfoDirectoryName/agreements.xml")
      _ <- stageBag.addMetadataFile(datasetXml, "dataset.xml")
      _ <- stageBag.addMetadataFile(filesXml, "files.xml")
      // TODO (1) notice that this only catches errors from giving the Runnable to the executor.
      //  In other words, errors from the job itself don't end up here.
      //  Find out which errors can occur in the job itself and make the job such that it will
      //  deal with those errors appropriately (setting state, etc.)
      // TODO (2) migrate 'workerActions' and functions that are called from there to 'SubmitJob'
      _ = executor.execute { workerActions(draftDeposit.id, draftBag, stageBag, submitDir) }
    } yield submittedId
  }

  // TODO a worker thread allows submit to return fast for large deposits.
  private def workerActions(id: UUID, draftBag: DansBag, stageBag: DansBag, submitDir: File): Runnable = () => for {
    // EASY-1464 3.3.8.b copy files
    _ <- stageBag.addPayloadFile(draftBag.data, Paths.get("."))
    _ <- stageBag.save()
    _ <- isValid(stageBag)
    // EASY-1464 3.3.7 checksums
    _ <- samePayloadManifestEntries(stageBag, draftBag)
    draftDepositDir = stageBag.baseDir.parent
    _ <- setRightsRecursively(draftDepositDir)
    // EASY-1464 step 3.3.9 Move copy to submit-to area
    _ = logger.info(s"moving $draftDepositDir to $submitDir")
    _ <- Try(draftDepositDir.moveTo(submitDir)(CopyOptions.atomically)).recoverWith {
      case _: FileAlreadyExistsException => Failure(AlreadySubmittedException(id))
    }
  } yield ()

  private def setRightsRecursively(file: File): Try[Unit] = {
    resource.managed(Files.walk(file.path, Int.MaxValue, VisitOptions.default: _*))
      .map(_.iterator().asScala.map(setRights).find(_.isFailure).getOrElse(Success(())))
      .tried
      .flatten
  }

  private def setRights(path: Path): Try[Unit] = Try {
    trace(path)
    // EASY-1932, variant of https://github.com/DANS-KNAW/easy-split-multi-deposit/blob/73189001217c2bf31b487eb8356f76ea4e9ffc31/src/main/scala/nl.knaw.dans.easy.multideposit/actions/SetDepositPermissions.scala#L72-L90
    val file = File(path)
    file.addPermission(GROUP_WRITE)
    file.addPermission(GROUP_READ)
    if (file.isDirectory)
      file.addPermission(GROUP_EXECUTE)
    // tried File(path).setGroup(groupPrincipal) but it causes "java.io.IOException: 'owner' parameter can't be a group"
    Files.getFileAttributeView(
      path,
      classOf[PosixFileAttributeView],
      LinkOption.NOFOLLOW_LINKS,
    ).setGroup(groupPrincipal)
  }.recoverWith {
    case e: FileSystemException => throw new IOException(s"Not able to set the group to ${ groupPrincipal.getName }. Probably the current user (${ System.getProperty("user.name") }) is not part of this group.", e)
    case e: IOException => throw new IOException(s"Could not set file permissions or group on $path", e)
    case e: SecurityException => throw new IOException(s"Not enough privileges to set file permissions or group on $path", e)
    case NonFatal(e) => throw new IOException(s"unexpected error occured on $path", e)
  }

  type ManifestItems = Map[File, String]
  type ManifestMap = Map[ChecksumAlgorithm, ManifestItems]

  private def sameFiles(payloadManifests: ManifestMap, dataDir: File): Try[Unit] = {
    val files = dataDir.walk().filter(!_.isDirectory).toSet
    payloadManifests.values.map(_.keySet)
      .find(_ != files)
      .map(manifestFiles => Failure(new Exception(s"invalid bag, missing [files, checksums]: [${ manifestFiles.diff(files) }, ${ files.diff(manifestFiles) }]")))
      .getOrElse(Success(()))
  }

  private def samePayloadManifestEntries(staged: DansBag, draft: DansBag) = {
    staged.payloadManifests.keySet.intersect(draft.payloadManifests.keySet)
      .map { algorithm =>
        val xs = getRelativeSet(staged, algorithm)
        val ys = getRelativeSet(draft, algorithm)
        (xs.diff(ys), ys.diff(xs))
      }
      .find(diffs => diffs._1.nonEmpty || diffs._2.nonEmpty)
      .map(diffs => Failure(new Exception(s"staged and draft bag [${ draft.baseDir.parent }] have different payload manifest elements: $diffs")))
      .getOrElse(Success(()))
  }

  private def getRelativeSet(bag: DansBag, algorithm: ChecksumAlgorithm): Set[(Path, String)] = {
    val baseDir = bag.baseDir
    bag.payloadManifests(algorithm).map {
      case (f: File, c: String) => (baseDir.relativize(f), c)
    }.toSet
  }

  private def isValid(stageBag: DansBag): Try[Unit] = stageBag.isValid match {
    case Left(msg) => Failure(new Exception(msg))
    case Right(_) => Success(())
  }
}
