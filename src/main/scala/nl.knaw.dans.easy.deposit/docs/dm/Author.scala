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
package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ SchemedKeyValue, SchemedValue }
import nl.knaw.dans.easy.deposit.docs.StringUtils._
import nl.knaw.dans.lib.string._

case class Author(titles: Option[String] = None,
                  initials: Option[String] = None,
                  insertions: Option[String] = None,
                  surname: Option[String] = None,
                  role: Option[SchemedKeyValue] = None,
                  ids: Option[Seq[SchemedValue]] = None,
                  organization: Option[String] = None,
                 ) extends Requirements {
  private val hasMandatory: Boolean = organization.isProvided || (surname.isProvided && initials.isProvided)
  private val hasRedundant: Boolean = !surname.isProvided && (titles.isProvided || insertions.isProvided)
  private val incompleteMsg = "Author needs one of (organisation | surname and initials)"
  private val redundantMsg = "Author has no surname so neither titles nor insertions"
  require(hasMandatory, buildMsg(incompleteMsg))
  require(!hasRedundant, buildMsg(redundantMsg))

  def isRightsHolder: Boolean = role.exists(_.key == "RightsHolder")

  override def toString: String = {
    def name = Seq(titles, initials, insertions, surname)
      .collect { case Some(s) if !s.isBlank => s }
      .mkString(" ")

    (surname.flatMap(_.toOption), organization.flatMap(_.toOption)) match {
      case (Some(_), Some(org)) => s"$name ($org)"
      case (None, Some(org)) => org
      case (Some(_), None) => name
      // only when requires is implemented incorrect:
      case (None, None) => throw new IllegalArgumentException(buildMsg(incompleteMsg))
    }
  }
}
