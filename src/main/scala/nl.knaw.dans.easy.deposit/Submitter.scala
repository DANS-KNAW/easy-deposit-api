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

import java.nio.file.attribute.GroupPrincipal

import better.files.File
import nl.knaw.dans.bag.ChecksumAlgorithm.ChecksumAlgorithm
import nl.knaw.dans.easy.deposit.Errors.InvalidDoiException
import nl.knaw.dans.easy.deposit.docs._
import nl.knaw.dans.easy.deposit.executor.JobQueueManager
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import org.joda.time.DateTime

import scala.util.{ Failure, Success, Try }

/**
 * Object that contains the logic for submitting a deposit.
 *
 * @param submitToBaseDir the directory to which the staged copy must be moved.
 */
class Submitter(submitToBaseDir: File,
                groupPrincipal: GroupPrincipal,
                depositUiURL: String,
                fileLimit: Int = 200,
                jobQueue: JobQueueManager,
                mailer: Mailer,
                agreementGenerator: AgreementGenerator,
               ) extends DebugEnhancedLogging {

  /**
   * Submits `depositDir` by writing the file metadata, updating the bag checksums, staging a copy
   * and moving that copy to the submit-to area.
   *
   * @param draftDeposit the deposit object to submit
   * @return the UUID of the deposit in the submit area (easy-ingest-flow-inbox)
   */
  def submit(draftDeposit: DepositDir, stateManager: StateManager, user: UserData, stagedDir: File): Try[Unit] = {
    trace(user, draftDeposit, stateManager, stagedDir)
    val depositId = draftDeposit.id
    logger.info(s"[$depositId] submitting deposit")
    for {
      draftBag <- draftDeposit.getDataFiles.map(_.bag)
      datasetMetadata <- draftDeposit.getDatasetMetadata
      agreementsXml <- AgreementsXml(DateTime.now, datasetMetadata, user).map(_.serialize)
      _ = logger.debug(agreementsXml)
      _ = datasetMetadata.doi.getOrElse(throw InvalidDoiException(depositId))
      _ <- draftDeposit.sameDOIs(datasetMetadata)
      datasetXml <- DDM(datasetMetadata).map(_.serialize)
      _ = logger.debug(datasetXml)
      filesXml <- FilesXml(draftBag.data).map(_.serialize)
      _ = logger.debug(filesXml)
      _ <- sameFiles(draftBag.payloadManifests, draftBag.baseDir / "data")
      // from now on no more user errors but internal errors
      _ = logger.info(s"[$depositId] dispatching submit action to threadpool executor")
      _ <- jobQueue.scheduleJob {
        new SubmitJob(
          depositId = draftDeposit.id,
          depositUiURL = depositUiURL,
          groupPrincipal = groupPrincipal,
          fileLimit = fileLimit,
          draftBag = draftBag,
          stagedDir = stagedDir,
          submitToBaseDir = submitToBaseDir,
          datasetXml = datasetXml,
          filesXml = filesXml,
          agreementsXml = agreementsXml,
          msg4DataManager = datasetMetadata.messageForDataManager.getOrElse("").stripLineEnd.toOption,
          agreementData = AgreementData(user, datasetMetadata),
          stateManager = stateManager,
          agreementGenerator = agreementGenerator,
          mailer = mailer,
        )
      }
    } yield ()
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
