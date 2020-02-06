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

import nl.knaw.dans.lib.string._

case class Author(titles: Option[String] = None,
                  initials: Option[String] = None,
                  insertions: Option[String] = None,
                  surname: Option[String] = None,
                  role: Option[SchemedKeyValue] = None,
                  ids: Option[Seq[SchemedValue]] = None,
                  organization: Option[String] = None,
                 ) extends OptionalValue {
  lazy val rightsHolder: Option[String] = role
    .filter(_.key.contains("RightsHolder"))
    .flatMap(_ => value)

  override lazy val value: Option[String] = {
    def name = Seq(titles, initials, insertions, surname)
      .collect { case Some(s) if !s.isBlank => s }
      .mkString(" ")

    (surname.flatMap(_.toOption), organization.flatMap(_.toOption)) match {
      case (Some(_), Some(org)) => s"$name ($org)"
      case (None, Some(org)) => org
      case (Some(_), None) => name
      case (None, None) => ""
    }
    }.toOption
  override lazy val hasValue: Boolean = value.exists(!_.isBlank)
}
