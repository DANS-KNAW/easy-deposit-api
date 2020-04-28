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
package nl.knaw.dans.easy.deposit.properties

import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.properties.DepositProperties.SubmittedProperties
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }

class ServiceDepositProperties(submitBase: File, override val depositId: UUID) extends DepositProperties with DebugEnhancedLogging {

  override def getSubmittedProperties: Try[SubmittedProperties] = {
    getSubmittedPropertiesFromService
      .flatMap {
        case Some(props) => Success(props)
        case None =>
          // if the deposit is moved to easy-ingest-flow, but is not picked up by that service yet,
          // read the properties from the file in easy-ingest-flow-inbox instead
          new FileDepositProperties(submitBase, depositId).getSubmittedProperties
      }
  }

  private def getSubmittedPropertiesFromService: Try[Option[SubmittedProperties]] = {
    // TODO implement GraphQL call to easy-deposit-properties
    ???
  }
}
