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

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import better.files.File
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.docs.{ AgreementsXml, DatasetXml, FilesXml, StateInfo }
import org.joda.time.DateTime

import scala.util.Try

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
    val stageDir = stagingBaseDir / depositDir.id.toString
    val stageMetadataDir = stageDir / "bag" / "metadata"
    val agreementsFile = stageMetadataDir / "agreements.xml"
    val datasetXmlFile = stageMetadataDir / "dataset.xml"
    val filesXmlFile = stageMetadataDir / "files.xml"
    val propsFileName = "deposit.properties"
    val submitted = StateInfo(StateInfo.State.submitted, "Deposit is ready for processing.")
    // TODO: implement as follows:
    for {
      // TODO cache json read (and possibly rewritten) by getDOI and  getDatasetMetadata?
      // EASY-1464 step 3.3.1 - 3.3.3
      _ <- depositDir.getDOI(pidRequester)
      // EASY-1464 step 3.3.4 validation
      //   [v] mandatory fields are present and not empty (by DatasetXml(datasetMetadata) in createXMLs)
      //   [v] DOI in json matches properties (by getDOI)
      //   [ ] URLs are valid
      //   [ ] ...
      // EASY-1464 3.3.5 part 1: generate xml file content for metadata
      dataFilesDir <- depositDir.getDataFiles.map(_.dataFilesBase)
      datasetMetadata <- depositDir.getDatasetMetadata // TODO skip recover: internal error if not catched by getDOI
      agreementsXml <- AgreementsXml(depositDir.user, DateTime.now, datasetMetadata)
      datasetXml <- DatasetXml(datasetMetadata)
      msg = datasetMetadata.messageForDataManager.getOrElse("")
      filesXml <- FilesXml(dataFilesDir)
      _ = stageDir.createDirectories()
      // EASY-1464 3.3.8.a
      stageBag <- DansV0Bag.empty(stageDir / "bag")
      // EASY-1464 3.3.6
      _ <- depositDir.setStateInfo(submitted)
      _ = stageBag.withEasyUserAccount(depositDir.user)
      _ = (dataFilesDir.parent.parent / propsFileName).copyTo(stageDir / propsFileName)
      // EASY-1464 3.3.5 part 2: write xml files to metadata // TODO sha's?
      _ = stageBag.addTagFile(msg.asInputStream)(_ / "metadata" / "message-from-depositor.txt")
      _ <- agreementsFile.writePretty(agreementsXml)
      _ <- datasetXmlFile.writePretty(datasetXml)
      _ <- filesXmlFile.writePretty(filesXml)
      // TODO: the next steps in a worker thread so that submit can return fast for large deposits.
      // EASY-1464 3.3.7 checksums +  3.3.8.b copy files
      _ <- dataFilesDir.children.failFastMap(f => stageBag.addPayloadFile(f)(_ / dataFilesDir.relativize(f).toString))
      _ <- stageBag.save() // TODO after each file to allow resume?
      // EASY-1464 step 3.3.9 Move copy to submit-to area
      //_ = stageDir.moveTo(submitToBaseDir / depositDir.id.toString)
    } yield ???
  }
}
