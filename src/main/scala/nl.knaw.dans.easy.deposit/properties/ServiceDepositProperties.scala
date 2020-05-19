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
import nl.knaw.dans.easy.deposit.properties.ServiceDepositProperties.GetSubmittedProperties
import nl.knaw.dans.easy.deposit.properties.graphql.GraphQLClient
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.JsonAST.JString
import org.json4s.{ Formats, JValue }

import scala.util.{ Failure, Success, Try }

class ServiceDepositProperties(submitBase: File, override val depositId: UUID, client: GraphQLClient)(implicit formats: Formats) extends DepositProperties with DebugEnhancedLogging {

  override def getSubmittedProperties: Try[SubmittedProperties] = {
    getSubmittedPropertiesFromService
      .flatMap {
        case Some(props) => Success(props)
        case None =>
          // if the deposit is moved to easy-ingest-flow, but is not picked up by that service yet,
          // so the deposit has not been registered in the properties-service yet,
          // read the properties from the file in easy-ingest-flow-inbox instead
          new FileDepositProperties(submitBase, depositId).getSubmittedProperties
      }
  }

  def getSubmittedPropertiesFromService: Try[Option[SubmittedProperties]] = {
    logger.info(s"[$depositId] calling easy-deposit-properties with operationName = ${ GetSubmittedProperties.operationName }")
    implicit val convertJson: Any => JValue = {
      case s: UUID => JString(s.toString)
    }
    for {
      json <- client.doQuery(GetSubmittedProperties.query, GetSubmittedProperties.operationName, Map("depositId" -> depositId)).toTry
      deposit = json.extract[GetSubmittedProperties.Data].deposit
      props <- deposit.map(deposit => {
        for {
          state <- deposit.state.map(Success(_))
            .getOrElse(Failure(NoStateForDeposit(depositId)))
          curationPerformed = deposit.curationPerformed.exists(_.value)
          fedoraIdentifier = deposit.identifier.map(_.value)
        } yield Some(SubmittedProperties(depositId, state.label, state.description, curationPerformed, fedoraIdentifier))
      }).getOrElse(Success(None))
    } yield props
  }
}

object ServiceDepositProperties {

  object GetSubmittedProperties {
    case class Data(deposit: Option[Deposit])
    case class Deposit(state: Option[State], curationPerformed: Option[IsCurationPerformed], identifier: Option[FedoraIdentifier])
    case class FedoraIdentifier(value: String)
    case class IsCurationPerformed(value: Boolean)
    case class State(label: String, description: String)

    val operationName = "GetSubmittedProperties"
    val query: String =
      """query GetSubmittedProperties($depositId: UUID!) {
        |  deposit(id: $depositId) {
        |    state {
        |      label
        |      description
        |    }
        |    curationPerformed {
        |      value
        |    }
        |    identifier(type: FEDORA) {
        |      value
        |    }
        |  }
        |}""".stripMargin
  }
}
