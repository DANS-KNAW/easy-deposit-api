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
import nl.knaw.dans.easy.deposit.docs.dm.Date.dateSubmitted
import nl.knaw.dans.easy.deposit.docs.dm.PrivacySensitiveDataPresent.PrivacySensitiveDataPresent
import nl.knaw.dans.easy.deposit.docs.dm._
import org.json4s.JsonInput

import scala.util.{ Failure, Success, Try }

/**
 * Params are (de)serialized by [[JsonUtil]].
 * Lazy public values remix the params for XML serialization with [[DDM]].
 */
case class DatasetMetadata(identifiers: Option[Seq[SchemedValue]] = None,
                           languageOfDescription: Option[SchemedKeyValue] = None,
                           titles: Option[Seq[String]] = None,
                           alternativeTitles: Option[Seq[String]] = None,
                           descriptions: Option[Seq[String]] = None,
                           creators: Option[Seq[Author]] = None,
                           contributors: Option[Seq[Author]] = None,
                           audiences: Option[Seq[SchemedKeyValue]] = None,
                           subjects: Option[Seq[PossiblySchemedKeyValue]] = None, //TODO xml
                           alternativeIdentifiers: Option[Seq[SchemedValue]] = None,
                           relations: Option[Seq[RelationType]] = None, //TODO xml
                           languagesOfFiles: Option[Seq[PossiblySchemedKeyValue]] = None,
                           dates: Option[Seq[Date]] = None,
                           sources: Option[Seq[String]] = None,
                           instructionsForReuse: Option[Seq[String]] = None,
                           publishers: Option[Seq[String]] = None,
                           accessRights: Option[AccessRights] = None,
                           license: Option[String] = None,
                           typesDcmi: Option[Seq[String]] = None,
                           types: Option[Seq[PossiblySchemedValue]] = None,
                           formats: Option[Seq[PossiblySchemedValue]] = None,
                           temporalCoverages: Option[Seq[PossiblySchemedKeyValue]] = None, //TODO xml
                           spatialPoints: Option[Seq[SpatialPoint]] = None,
                           spatialBoxes: Option[Seq[SpatialBox]] = None,
                           spatialCoverages: Option[Seq[PossiblySchemedKeyValue]] = None, //TODO xml
                           messageForDataManager: Option[String] = None,
                           privacySensitiveDataPresent: PrivacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified,
                           acceptLicenseAgreement: Boolean = false,
                          ) {
  lazy val hasPrivacyData: Try[Boolean] = privacySensitiveDataPresent match {
    case PrivacySensitiveDataPresent.yes => Success(true)
    case PrivacySensitiveDataPresent.no => Success(false)
    case PrivacySensitiveDataPresent.unspecified => Failure(missingValue("PrivacySensitiveDataPresent"))
  }

  lazy val licenceAccepted: Try[Unit] = if (acceptLicenseAgreement) Success(())
                                        else Failure(missingValue("AcceptLicenseAgreement"))

  private val specialDateQualifiers = Seq(DateQualifier.created, DateQualifier.available)
  private val flattenedDates: Seq[Date] = dates.toSeq.flatten
  require(!flattenedDates.exists(_.qualifier == DateQualifier.dateSubmitted), s"No ${ DateQualifier.dateSubmitted } allowed")

  lazy val datesCreated: Seq[Date] = flattenedDates.filter(_.qualifier == DateQualifier.created)
  lazy val datesAvailable: Seq[Date] = flattenedDates.filter(_.qualifier == DateQualifier.available)
  lazy val otherDates: Seq[Date] = flattenedDates.filterNot(date =>
    specialDateQualifiers.contains(date.qualifier)
  ) :+ dateSubmitted()

  lazy val (rightsHoldingCreators, creatorsWithoutRights) = creators.getOrElse(Seq.empty).partition(_.isRightsHolder)
  lazy val (rightsHoldingContributors, contributorsWithoutRights) = contributors.getOrElse(Seq.empty).partition(_.isRightsHolder)
  lazy val rightsHolders: Seq[Author] = rightsHoldingContributors ++ rightsHoldingCreators

  lazy val allIdentifiers: Seq[SchemedValue] = identifiers.getOrElse(Seq()) ++ alternativeIdentifiers.getOrElse(Seq())
  lazy val allTypes: Seq[PossiblySchemedValue] = types.getOrElse(Seq()) ++ typesDcmi.getOrElse(Seq()).map(
    PossiblySchemedValue(Some("dcterms:DCMIType"), _)
  )

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

  implicit class OptionalString[T](val value: Option[T]) extends AnyVal {
    def isProvided: Boolean = value match {
      case Some(str: String) => str.trim.nonEmpty
      case _ => value.isDefined
    }
  }

  trait RequiresNonEmpty {
    def requireNonEmptyString[T](value: T, tag: String): Unit = {
      value match {
        case str: String =>
          require(str.trim.nonEmpty, s"empty $tag provided for ${ this.getClass.getSimpleName } $this")
        case _ =>
      }
    }
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
                         ) extends RequiresNonEmpty {
    requireNonEmptyString(scheme, "scheme")
    requireNonEmptyString(value, "value")
  }

  case class PossiblySchemedValue(override val scheme: Option[String],
                                  value: String,
                                 ) extends PossiblySchemed with RequiresNonEmpty {
    requireNonEmptyString(value, "value")
  }

  case class SchemedKeyValue(scheme: String,
                             key: String,
                             value: String,
                            ) extends RequiresNonEmpty {
    requireNonEmptyString(scheme, "scheme")
    requireNonEmptyString(value, "value")
    requireNonEmptyString(key, "key")
  }

  case class PossiblySchemedKeyValue(override val scheme: Option[String],
                                     key: Option[String],
                                     value: String,
                                    ) extends PossiblySchemed with RequiresNonEmpty {
    requireNonEmptyString(value, "value")
  }
}

