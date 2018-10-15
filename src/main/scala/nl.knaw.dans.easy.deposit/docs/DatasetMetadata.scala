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
package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, RichJsonInput }
import nl.knaw.dans.easy.deposit.docs.dm.Date.{ atMostOne, dateSubmitted, notAllowed }
import nl.knaw.dans.easy.deposit.docs.dm.PrivacySensitiveDataPresent.PrivacySensitiveDataPresent
import nl.knaw.dans.easy.deposit.docs.dm._
import org.json4s.JsonInput

import scala.util.{ Failure, Success, Try }

/**
 * All params are (de)serialized by [[JsonUtil]].
 * Private params are remixed or converted for XML serialization with [[DDM]].
 */
case class DatasetMetadata(private val identifiers: Option[Seq[SchemedValue]] = None,
                           languageOfDescription: Option[SchemedKeyValue] = None,
                           titles: Option[Seq[String]] = None,
                           alternativeTitles: Option[Seq[String]] = None,
                           descriptions: Option[Seq[String]] = None,
                           creators: Option[Seq[Author]] = None,
                           contributors: Option[Seq[Author]] = None,
                           audiences: Option[Seq[SchemedKeyValue]] = None,
                           subjects: Option[Seq[PossiblySchemedKeyValue]] = None, //TODO xml
                           private val alternativeIdentifiers: Option[Seq[SchemedValue]] = None,
                           relations: Option[Seq[RelationType]] = None,
                           languagesOfFiles: Option[Seq[PossiblySchemedKeyValue]] = None,
                           private val dates: Option[Seq[Date]] = None,
                           sources: Option[Seq[String]] = None,
                           instructionsForReuse: Option[Seq[String]] = None,
                           publishers: Option[Seq[String]] = None,
                           accessRights: Option[AccessRights] = None,
                           license: Option[String] = None,
                           private val typesDcmi: Option[Seq[String]] = None,
                           private val types: Option[Seq[PossiblySchemedValue]] = None,
                           formats: Option[Seq[PossiblySchemedValue]] = None,
                           temporalCoverages: Option[Seq[PossiblySchemedKeyValue]] = None, //TODO xml
                           spatialPoints: Option[Seq[SpatialPoint]] = None,
                           spatialBoxes: Option[Seq[SpatialBox]] = None,
                           spatialCoverages: Option[Seq[PossiblySchemedKeyValue]] = None, //TODO xml
                           messageForDataManager: Option[String] = None,
                           private val privacySensitiveDataPresent: PrivacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified,
                           acceptLicenseAgreement: Boolean = false,
                          ) {
  lazy val hasPrivacySensitiveData: Try[Boolean] = privacySensitiveDataPresent match {
    case PrivacySensitiveDataPresent.yes => Success(true)
    case PrivacySensitiveDataPresent.no => Success(false)
    case PrivacySensitiveDataPresent.unspecified => Failure(missingValue("PrivacySensitiveDataPresent"))
  }

  lazy val licenceAccepted: Try[Unit] = if (acceptLicenseAgreement) Success(())
                                        else Failure(missingValue("AcceptLicenseAgreement"))

  //// dates
  private val specialDateQualifiers = Seq(DateQualifier.created, DateQualifier.available)
  private val (specialDates, plainDates) = dates.toSeq.flatten
    .partition(date => specialDateQualifiers.contains(date.qualifier))
  val (datesCreated, datesAvailable) = specialDates
    .partition(_.qualifier == DateQualifier.created)
  val otherDates: Seq[Date] = plainDates :+ dateSubmitted()
  // N.B: with lazy values JsonUtil.deserialize would not throw exceptions
  notAllowed(DateQualifier.dateSubmitted, plainDates)
  atMostOne(datesCreated)
  atMostOne(datesAvailable)

  //// authors
  lazy val rightsHolders: Seq[Author] =
    contributors.getOrElse(Seq.empty).filter(_.isRightsHolder) ++
      creators.getOrElse(Seq.empty).filter(_.isRightsHolder)

  lazy val allIdentifiers: Seq[SchemedValue] = identifiers.getOrElse(Seq()) ++ alternativeIdentifiers.getOrElse(Seq())

  lazy val allTypes: Seq[PossiblySchemedValue] = types.getOrElse(Seq()) ++ typesDcmi.getOrElse(Seq()).map(
    PossiblySchemedValue(Some("dcterms:DCMIType"), _)
  )

  //// doi
  lazy val doi: Option[String] = identifiers.flatMap(_.collectFirst {
    case SchemedValue(`doiScheme`, value) => value
  })

  def setDoi(value: String): DatasetMetadata = {
    val ids = identifiers.getOrElse(Seq.empty).filterNot(_.scheme == doiScheme)
    this.copy(identifiers = Some(ids :+ SchemedValue(doiScheme, value)))
  }
}

object DatasetMetadata {
  def apply(input: JsonInput): Try[DatasetMetadata] = input.deserialize[DatasetMetadata]

  val doiScheme = "id-type:DOI"

  def missingValue(label: String): InvalidDocumentException = {
    InvalidDocumentException("DatasetMetadata", new Exception(s"Please set $label"))
  }

  trait PossiblySchemed {
    val scheme: Option[String]

    def schemeAsString: String = scheme match {
      case Some(s: String) if s.trim.nonEmpty => s.trim
      case _ => null // will suppress the XML attribute
    }
  }

  case class SchemedValue(scheme: String,
                          value: String,
                         ) extends Requirements {
    requireNonEmptyString(scheme)
    requireNonEmptyString(value)
  }

  case class PossiblySchemedValue(override val scheme: Option[String],
                                  value: String,
                                 ) extends PossiblySchemed with Requirements {
    requireNonEmptyString(value)
  }

  case class SchemedKeyValue(scheme: String,
                             key: String,
                             value: String,
                            ) extends Requirements {
    requireNonEmptyString(scheme)
    requireNonEmptyString(value)
    requireNonEmptyString(key)
  }

  case class PossiblySchemedKeyValue(override val scheme: Option[String],
                                     key: Option[String],
                                     value: String,
                                    ) extends PossiblySchemed with Requirements {
    requireNonEmptyString(value)
  }
}

