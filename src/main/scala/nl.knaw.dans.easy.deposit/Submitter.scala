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

import java.nio.file.{ Path, Paths }

import better.files.File
import nl.knaw.dans.bag.ChecksumAlgorithm.ChecksumAlgorithm
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.docs.{ AgreementsXml, DatasetXml, FilesXml, StateInfo }
import org.joda.time.DateTime

import scala.util.{ Failure, Success, Try }

/**
 * Object that contains the logic for submitting a deposit.
 *
 * @param stagingBaseDir  the base directory for staged copies
 * @param submitToBaseDir the directory to which the staged copy must be moved.
 */
class Submitter(stagingBaseDir: File,
                submitToBaseDir: File,
                pidRequester: PidRequester) {

  /**
   * Submits `depositDir` by writing the file metadata, updating the bag checksums, staging a copy
   * and moving that copy to the submit-to area.
   *
   * @param depositDir the deposit directory to submit
   * @return
   */
  def submit(depositDir: DepositDir): Try[Unit] = {
    val propsFileName = "deposit.properties"
    println(s"submitter: pidRequester =  $pidRequester")
    for {
      // TODO cache json read (and possibly rewritten) by getDOI and  getDatasetMetadata?
      // EASY-1464 step 3.3.1 - 3.3.3
      _ <- depositDir.getDOI(pidRequester)
      // EASY-1464 step 3.3.4 validation
      //   [v] mandatory fields are present and not empty (by DatasetXml(datasetMetadata) in createXMLs)
      //   [v] DOI in json matches properties (by getDOI)
      //   [ ] URLs are valid
      //   [ ] ...
      // EASY-1464 3.3.5.a: generate (with some implicit validation) content for metadata files
      draftBag <- depositDir.getDataFiles.map(_.bag)
      datasetMetadata <- depositDir.getDatasetMetadata // TODO skip recover: internal error if not catched by getDOI
      agreementsXml <- AgreementsXml(depositDir.user, DateTime.now, datasetMetadata)
      datasetXml <- DatasetXml(datasetMetadata)
      msg = datasetMetadata.messageForDataManager.getOrElse("")
      filesXml <- FilesXml(draftBag.data)
      _ <- sameFiles(draftBag.payloadManifests, draftBag.baseDir / "data")
      // TODO this is the last moment to report an invalid DDM as user error, subsequent errors are internal server errors
      // EASY-1464 3.3.8.a create empty staged bag to take a copy of the deposit
      stageDir = (stagingBaseDir / depositDir.id.toString).createDirectories()
      stageBag <- DansV0Bag.empty(stageDir / "bag").map(_.withEasyUserAccount(depositDir.user))
      // EASY-1464 3.3.6 change state and copy with the rest of the deposit properties to staged dir
      _ <- depositDir.setStateInfo(StateInfo(StateInfo.State.submitted, "Deposit is ready for processing."))
      _ = (draftBag.baseDir.parent / propsFileName).copyTo(stageDir / propsFileName)
      // EASY-1464 3.3.5.b: write files to metadata
      _ = stageBag.addMetadataFile(msg, "message-from-depositor.txt")
      _ <- stageBag.addMetadataFile(agreementsXml, "agreements.xml")
      _ <- stageBag.addMetadataFile(datasetXml, "dataset.xml")
      _ <- stageBag.addMetadataFile(filesXml, "files.xml")
      _ <- workerActions(draftBag, stageBag, submitToBaseDir / depositDir.id.toString)
    } yield ()
  }

  // TODO a worker thread allows submit to return fast for large deposits.
  private def workerActions(draftBag: DansBag, stageBag: DansBag, submitDir: File) = for {
    // EASY-1464 3.3.8.b copy files
    _ <- stageBag.addPayloadFile(draftBag.data, Paths.get("."))
    _ <- stageBag.save()
    _ <- isValid(stageBag)
    // EASY-1464 3.3.7 checksums
    _ <- samePayloadManifestEntries(stageBag, draftBag)
    // EASY-1464 step 3.3.9 Move copy to submit-to area
    _ = stageBag.baseDir.parent.moveTo(submitDir)
  } yield ()

  type ManifestItems = Map[File, String]
  type ManifestMap = Map[ChecksumAlgorithm, ManifestItems]

  private def sameFiles(payloadManifests: ManifestMap, dataDir: File) = {
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
      .map(diffs => Failure(new Exception(s"staged and draft bag [${draft.baseDir.parent}] have different payload manifest elements: $diffs")))
      .getOrElse(Success(()))
  }

  private def getRelativeSet(bag: DansBag, algorithm: ChecksumAlgorithm): Set[(Path, String)] = {
    val baseDir = bag.baseDir
    bag.payloadManifests(algorithm).map {
      case (f: File, c: String) => (baseDir.relativize(f), c)
    }.toSet
  }

  private def isValid(stageBag: DansBag) = stageBag.isValid match {
    case Left(msg) => Failure(new Exception(msg))
    case Right(_) => Success(())
  }
}
