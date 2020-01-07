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
import java.nio.file.attribute.UserPrincipalNotFoundException
import java.util.UUID

import better.files.File
import nl.knaw.dans.bag.ChecksumAlgorithm.ChecksumAlgorithm
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.Errors.{ AlreadySubmittedException, InvalidDoiException }
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs._
import nl.knaw.dans.easy.deposit.executor.JobQueueManager
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import org.joda.time.DateTime

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
                fileLimit: Int = 200,
                jobQueue: JobQueueManager,
                mailer: Mailer,
                agreementGenerator: AgreementGenerator,
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
  def submit(draftDeposit: DepositDir, stateManager: StateManager, user: UserData, stagedDir: File): Try[UUID] = {
    trace(user, draftDeposit, stateManager, stagedDir)
    val propsFileName = "deposit.properties"
    val depositId = draftDeposit.id
    logger.info(s"[$depositId] submitting deposit")
    for {
      draftBag <- draftDeposit.getDataFiles.map(_.bag)
      datasetMetadata <- draftDeposit.getDatasetMetadata
      agreementsXmlElem <- AgreementsXml(DateTime.now, datasetMetadata, user)
      agreementsXml = agreementsXmlElem.serialize
      _ = logger.debug(agreementsXml)
      _ = datasetMetadata.doi.getOrElse(throw InvalidDoiException(depositId))
      _ <- draftDeposit.sameDOIs(datasetMetadata)
      datasetXmlElem <- DDM(datasetMetadata)
      datasetXml = datasetXmlElem.serialize
      _ = logger.debug(datasetXml)
      (maybeFirstPartOfMsg4DataManager, msg4DataManager) = {
        val firstPart = datasetMetadata.messageForDataManager.getOrElse("").stripLineEnd.toOption
        val secondPart = s"The deposit can be found at $depositUiURL/$depositId"
        firstPart -> firstPart.fold(secondPart)(_ + "\n\n" + secondPart)
      }
      _ = logger.debug("Message for the datamanager: " + msg4DataManager)
      filesXmlElem <- FilesXml(draftBag.data)
      filesXml = filesXmlElem.serialize
      _ = logger.debug(filesXml)
      _ <- sameFiles(draftBag.payloadManifests, draftBag.baseDir / "data")
      // from now on no more user errors but internal errors
      stageBag <- DansV0Bag.empty(stagedDir / bagDirName).map(_.withCreated())
      oldStateInfo <- stateManager.getStateInfo
      newStateInfo = StateInfo(State.submitted, "The deposit is being processed")
      _ <- stateManager.changeState(oldStateInfo, newStateInfo)
      submittedId <- stateManager.getSubmittedBagId // created by changeState
      submitDir = submitToBaseDir / submittedId.toString
      _ = if (submitDir.exists) throw AlreadySubmittedException(depositId)
      _ = (draftBag.baseDir.parent / propsFileName).copyTo(stagedDir / propsFileName)
      _ = stageBag.addMetadataFile(msg4DataManager, s"$depositorInfoDirectoryName/message-from-depositor.txt")
      _ <- stageBag.addMetadataFile(agreementsXml, s"$depositorInfoDirectoryName/agreements.xml")
      _ <- stageBag.addMetadataFile(datasetXml, "dataset.xml")
      _ <- stageBag.addMetadataFile(filesXml, "files.xml")
      agreementData = AgreementData(user, datasetMetadata)
      _ = logger.info(s"[$depositId] dispatching submit action to threadpool executor")
      _ <- jobQueue.scheduleJob {
        new SubmitJob(
          depositId = draftDeposit.id,
          serializedDatasetXml = datasetXml,
          msg4Datamanager = maybeFirstPartOfMsg4DataManager,
          draftBag = draftBag,
          stageBag = stageBag,
          submitDir = submitDir,
          groupPrincipal = groupPrincipal,
          agreementData = agreementData,
          agreementGenerator = agreementGenerator,
          fileLimit = 200,
          mailer = mailer,
          stateManager = stateManager,
        )
      }
    } yield submittedId
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
}
