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
package nl.knaw.dans.easy

import java.nio.file.Paths
import java.util.UUID

import better.files.StringOps
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State

import scala.util.Try
import scala.xml._

package object deposit {

  sealed abstract class DepositException(msg: String, cause: Throwable) extends Exception(msg, cause)

  case class NoSuchDepositException(user: String, id: UUID, cause: Throwable)
    extends DepositException(s"Deposit with id $id not found for user $user:  ${ cause.getMessage }", cause)

  case class CorruptDepositException(user: String, id: String, cause: Throwable)
    extends DepositException(s"Invalid deposit uuid $id for user $user: ${ cause.getMessage }", cause)

  case class IllegalStateTransitionException(user: String, id: UUID, oldState: State, newState: State)
    extends DepositException(s"Cannot transition from $oldState to $newState (deposit id: $id, user: $user)", null)

  case class ConfigurationException(msg: String) extends IllegalArgumentException(s"Configuration error: $msg")
  case class InvalidDoiException(uuid: UUID) extends Exception(s"InvalidDoi: DOI must be obtained by calling GET /deposit/$uuid")

  val prologue = """<?xml version='1.0' encoding='UTF-8'?>"""

  implicit class XmlExtensions(val elem: Elem) extends AnyVal {

    def serialize: String = {
      val printer = new PrettyPrinter(160, 2)
      val trimmed = Utility.trim(elem)
      prologue + "\n" + printer.format(trimmed)
    }
  }
  implicit class BagExtensions(val bag: DansV0Bag) extends AnyVal {
    def addMetadataFile(content: Elem, target: String): Try[Any] = {
      bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/$target"))
    }

    def addMetadataFile(content: String, target: String): Try[Any] = {
      bag.addTagFile(content.inputStream, Paths.get(s"metadata/$target"))
    }
  }
}
