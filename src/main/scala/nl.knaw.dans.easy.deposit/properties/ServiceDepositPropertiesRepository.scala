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
import nl.knaw.dans.easy.deposit.properties.graphql.GraphQLClient
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.Formats

import scala.util.Try

class ServiceDepositPropertiesRepository(submitBase: File, client: GraphQLClient)(implicit formats: Formats) extends DepositPropertiesRepository with DebugEnhancedLogging {

  override def load(depositId: UUID): Try[DepositProperties] = Try {
    new ServiceDepositProperties(submitBase, depositId, client)
  }
}
