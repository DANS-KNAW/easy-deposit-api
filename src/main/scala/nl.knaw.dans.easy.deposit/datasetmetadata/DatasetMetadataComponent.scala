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
package nl.knaw.dans.easy.deposit.datasetmetadata

import java.util.UUID

import scala.util.Try


trait DatasetMetadataComponent {

  trait DatasetMetadata {

    /**
     * Returns the dataset metadata from `dataset.xml`.
     *
     * @param user the user ID
     * @param id the deposit ID
     * @return
     */
    def getDatasetMetadataForDeposit(user: String, id: UUID): Try[DatasetMetadata] = ???

    /**
     * Writes the provided [[DatasetMetadata]] object as `dataset.xml` to the deposit directory. Any
     * existing `dataset.xml` is overwritten, so it is important that the object contains the complete
     * current metadata.
     *
     * @param user the user ID
     * @param uuid the deposit ID
     * @param dm the dataset metadata
     * @return
     */
    def writeDataMetadataToDeposit(user: String, uuid: UUID, dm: DatasetMetadata): Try[Unit] = ???
  }
}
