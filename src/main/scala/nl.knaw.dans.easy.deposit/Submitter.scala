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

import better.files.File
import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
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
    // TODO: implement as follows:
    for {
      // TODO cache json read (and possibly rewritten) by getDOI for createXMLs?
      _ <- depositDir.getDOI(pidRequester) // EASY-1464 step 3.3.1 - 3.3.3
      // EASY-1464 step 3.3.4 validation
      //   [v] mandatory fields are present and not empty (by DatasetXml(datasetMetadata) in createXMLs)
      //   [v] DOI in json matches properties (by getDOI)
      //   [ ] URLs are valid
      //   [ ] ...
      _ <- depositDir.createXMLs(DateTime.now) // EASY-1464 3.3.5
      // EASY-1464 step 3.3.6 Set state to SUBMITTED
      // EASY-1464 step 3.3.7 Update/write bag checksums.
      // EASY-1464 step 3.3.8 Copy to staging area
      // EASY-1464 step 3.3.9 Move copy to submit-to area
    } yield ???
  }
}
