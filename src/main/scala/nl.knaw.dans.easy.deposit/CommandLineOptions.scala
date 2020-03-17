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

import java.util.UUID

import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.StateInfo.State._
import org.rogach.scallop.{ ScallopConf, ScallopOption, Subcommand, ValueConverter, singleArgConverter, stringConverter }

class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "easy-deposit-api"
  version(configuration.version)
  private val SUBCOMMAND_SEPARATOR = "---\n"
  val description: String = s"""JSON-based deposit API"""
  val synopsis: String =
    s"""
       |  $printedName run-service
       |  $printedName change-state [ --doUpdate ] --label <value> --description <string> <user> <depositId>""".stripMargin

  version(s"$printedName v${ configuration.version }")
  banner(
    s"""
       |  $description
       |
       |Usage:
       |
       |$synopsis
       |
       |Options:
       |""".stripMargin)

  private implicit val stateLabelParser: ValueConverter[State] = singleArgConverter[State](State.withName)
  private implicit val uuidParser: ValueConverter[UUID] = singleArgConverter[UUID](UUID.fromString)

  val runService: Subcommand = new Subcommand("run-service") {
    descr(
      "Starts EASY Deposit Api as a daemon that services HTTP requests")
    footer(SUBCOMMAND_SEPARATOR)
  }
  val changeState = new Subcommand("change-state") {
    descr(
      "Changes the state of a deposit, when changing to SUBMITTED just the state is changed, the rest of the submit-cycle is not started")
    val doUpdate: ScallopOption[Boolean] = opt(name = "doUpdate", noshort = true, required = false,
      descr = s"without this argument only the current status is shown in json format")
    val state: ScallopOption[State] = opt(name = "label", short = 'l', required = true,
      descr = s"The label of the new state, one of: ${State.values.mkString(", ")}")
    val description: ScallopOption[String] = opt(name = "description", short = 'd', required = true,
      descr = "A desription of the new state")
    val draftOwnerId: ScallopOption[String] = trailArg(name = "draftOwnerId", required = true,
      descr = "The owner of the existing draft deposit")
    val draftDepositId: ScallopOption[UUID] = trailArg(name = "draftDepositId", required = true,
      descr = "The UUID of the existing draft deposit")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(runService)
  addSubcommand(changeState)

  footer("")
  verify()
}
