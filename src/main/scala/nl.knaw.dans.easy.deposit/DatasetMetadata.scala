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

import nl.knaw.dans.easy.deposit.DatasetMetadata._

case class DatasetMetadata(doi: Option[String] = None,
                           urn: Option[String] = None,
                           titles: Seq[String] = Seq.empty,
                           alternativeTitles: Seq[String] = Seq.empty,
                           creators: Option[Object] = None,
                           contributors: Option[Object] = None,
                           created: Option[String] = None,
                           descriptions: Seq[String] = Seq.empty,
                           audiences: Seq[String] = Seq.empty,
                           archisNrs: Seq[String] = Seq.empty,
                           subjectsAbrComplex: Seq[String] = Seq.empty,
                           subjects: Seq[String] = Seq.empty,
                           temporalCoveragesAbr: Seq[String] = Seq.empty,
                           temporalCoverages: Seq[String] = Seq.empty,
                           spatialPoints: Option[Object] = None,
                           spatialBoxes: Option[Object] = None,
                           spatialCoverages: Seq[String] = Seq.empty,
                           identifiers: Option[Seq[Identifier]] = None,
                           relations: Option[Seq[Relation]] = None,
                           typesDcmi: Seq[String] = Seq.empty,
                           types: Seq[String] = Seq.empty,
                           formatsMediaType: Seq[String] = Seq.empty,
                           formats: Seq[String] = Seq.empty,
                           languagesIso639: Seq[String] = Seq.empty,
                           languages: Seq[String] = Seq.empty,
                           sources: Seq[String] = Seq.empty,
                           datesIso8601: Option[Seq[Date]] = None,
                           dates: Option[Seq[Date]] = None,
                           remarks: Option[String] = None,
                           rightsHolders: Seq[String] = Seq.empty,
                           publishers: Seq[String] = Seq.empty,
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
