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
import nl.knaw.dans.easy.deposit.properties.FileDepositProperties.depositPropertiesFileName
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.lang.BooleanUtils

import scala.util.Try

class FileDepositProperties(submitBase: File, override val depositId: UUID) extends DepositProperties with DebugEnhancedLogging {
  private lazy val deposit = submitBase / depositId.toString
  private lazy val depositProperties: Try[PropertiesConfiguration] = findDepositProperties

  private def depositPropertiesFilePath: File = deposit / depositPropertiesFileName

  private def findDepositProperties: Try[PropertiesConfiguration] = Try {
    if (depositPropertiesFilePath.exists) {
      debug(s"Getting info from $deposit")
      new PropertiesConfiguration() {
        setDelimiterParsingDisabled(true)
        setFile(depositPropertiesFilePath.toJava)
        load(depositPropertiesFilePath.toJava)
      }
    }
    else
      new PropertiesConfiguration() // when the next request is done before the deposit is moved to `submitBase`.
  }

  override def getSubmittedProperties: Try[SubmittedProperties] = {
    depositProperties.map(props => SubmittedProperties(
      depositId = depositId,
      stateLabel = props.getString("state.label"),
      stateDescription = props.getString("state.description"),
      curationPerformed = Option(props.getString("curation.performed")).exists(BooleanUtils.toBoolean),
      fedoraId = Option(props.getString("identifier.fedora")),
    ))
  }
}

object FileDepositProperties {
  val depositPropertiesFileName = "deposit.properties"
}
