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

import java.nio.charset.Charset
import java.nio.file.Path
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State

import scala.util.Try
import scala.xml.{ Elem, PrettyPrinter, Utility, XML }

package object deposit {

  sealed abstract class DepositException(msg: String, cause: Throwable) extends Exception(msg, cause)

  case class NoSuchDepositException(user: String, id: UUID, cause: Throwable)
    extends DepositException(s"Deposit with id $id not found for user $user:  ${ cause.getMessage }", cause)

  case class CorruptDepositException(user: String, id: String, cause: Throwable)
    extends DepositException(s"Invalid deposit uuid $id for user $user: ${ cause.getMessage }", cause)

  case class IllegalStateTransitionException(user: String, id: UUID, oldState: State, newState: State)
    extends DepositException(s"Cannot transition from $oldState to $newState (deposit id: $id, user: $user)", null)

  case class ConfigurationException(msg: String) extends IllegalArgumentException(s"Configuration error: $msg")

  /**
   * Information about a file in the deposit
   *
   * @param fileName the simple filename of the file
   * @param dirPath  path of the containing directory, relative to the content base directory
   * @param sha1sum  the SHA-1 checksum of the file data
   */
  case class FileInfo(fileName: String, dirPath: Path, sha1sum: String)

  implicit class FileExtensions(val file: File) extends AnyVal {
    def writePretty(elem: Elem): Try[Unit] = Try {
      val pretty = XML.loadString(new PrettyPrinter(160, 2).format(Utility.trim(elem)))
      XML.save(file.toString, pretty, Charset.forName("UTF-8").toString, xmlDecl = true)
    }
  }
}
