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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ SchemedKeyValue, SchemedValue, _ }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson

case class Author(titles: Option[String] = None,
                  initials: Option[String] = None,
                  insertions: Option[String] = None,
                  surname: Option[String] = None,
                  role: Option[SchemedKeyValue] = None,
                  ids: Option[Seq[SchemedValue]] = None,// TODO xml
                  organization: Option[String] = None,
                 ) {
  private val hasMandatory: Boolean = organization.isProvided || (surname.isProvided && initials.isProvided)
  private val hasRedundant: Boolean = surname.isEmpty && (titles.isProvided || insertions.isProvided)
  private val incompleteMsg = "needs one of (organisation | surname and initials)"
  private val redundantMsg = "without surname should have neither titles nor insertions"
  require(hasMandatory, buildMsg(incompleteMsg))
  require(!hasRedundant, buildMsg(redundantMsg))

  private def buildMsg(s: String) = s"Author $s; got: ${ toJson(this) }"

  def isRightsHolder: Boolean = role.exists(_.key == "RightsHolder")

  override def toString: String = { // TODO ID's for rightsHolders
    val name = Seq(titles, initials, insertions, surname)
      .filter(_.isProvided)
      .map(_.getOrElse(""))
      .mkString(" ")
    (surname.isProvided, organization.isProvided) match {
      case (true, true) => s"$name; ${ organization.getOrElse("") }"
      case (false, true) => organization.getOrElse("")
      case (true, false) => name
      case (false, false) => throw new Exception(buildMsg(incompleteMsg)) // only with wrong requires
    }
  }
}
