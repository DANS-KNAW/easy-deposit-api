/**
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License] = None, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing] = None, software
 * distributed under the License is distributed on an "AS IS" BASIS] = None,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND] = None, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.deposit

import nl.knaw.dans.easy.deposit.DatasetMetadata._

case class DatasetMetadata(doi: Option[String] = None,
                           urn: Option[String] = None,
                           titles: Option[Seq[String]] = None,
                           alternativeTitles: Option[Seq[String]] = None,
                           creators: Option[Object] = None,
                           contributors: Option[Object] = None,
                           created: Option[String] = None,
                           descriptions: Option[Seq[String]] = None,
                           audiences: Option[Seq[String]] = None,
                           archisNrs: Option[Seq[String]] = None,
                           subjectsAbrComplex: Option[Seq[String]] = None,
                           subjects: Option[Seq[String]] = None,
                           temporalCoveragesAbr: Option[Seq[String]] = None,
                           temporalCoverages: Option[Seq[String]] = None,
                           spatialPoints: Option[Object] = None,
                           spatialBoxes: Option[Object] = None,
                           spatialCoverages: Option[Seq[String]] = None,
                           identifiers: Option[Seq[Identifier]] = None,
                           relations: Option[Seq[Relation]] = None,
                           typesDcmi: Option[Seq[String]] = None,
                           types: Option[Seq[String]] = None,
                           formatsMediaType: Option[Seq[String]] = None,
                           formats: Option[Seq[String]] = None,
                           languagesIso639: Option[Seq[String]] = None,
                           languages: Option[Seq[String]] = None,
                           sources: Option[Seq[String]] = None,
                           datesIso8601: Option[Seq[Date]] = None,
                           dates: Option[Seq[Date]] = None,
                           remarks: Option[String] = None,
                           rightsHolders: Option[Seq[String]] = None,
                           publishers: Option[Seq[String]] = None,
                           dateAvailable: Option[String] = None,
                          ) {

}
object DatasetMetadata {
  case class Author(
                     titles: Option[String] = None,
                     initials: Option[String] = None,
                     insertions: Option[String] = None,
                     surname: Option[String] = None,
                     role: Option[String] = None,
                     dai: Option[String] = None,
                     organization: Option[String] = None,
                   )

  case class Date(scheme: Option[String] = None,
                  date: Option[String] = None,
                 )

  case class SpatialPoint(scheme: Option[String] = None,
                          x: Option[String] = None,
                          y: Option[String] = None,
                         )

  case class SpatialBox(scheme: Option[String] = None,
                        north: Option[String] = None,
                        east: Option[String] = None,
                        south: Option[String] = None,
                        west: Option[String] = None,
                       )

  case class Identifier(scheme: Option[String] = None,
                        identifier: Option[String] = None,
                       )

  case class Relation(qualifier: Option[String] = None,
                      url: Option[String] = None,
                      title: Option[String] = None,
                     )
}
