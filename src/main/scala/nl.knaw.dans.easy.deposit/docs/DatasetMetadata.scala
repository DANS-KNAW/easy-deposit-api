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

import nl.knaw.dans.easy.deposit.Errors.InvalidDocumentException
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.RichJsonInput
import nl.knaw.dans.easy.deposit.docs.dm.AccessRights.AccessRights
import nl.knaw.dans.easy.deposit.docs.dm.Date._
import nl.knaw.dans.easy.deposit.docs.dm.PrivacySensitiveDataPresent.PrivacySensitiveDataPresent
import nl.knaw.dans.easy.deposit.docs.dm._
import org.json4s.JsonInput

import scala.util.{ Failure, Success, Try }

/**
 * All params are (de)serialized by [[JsonUtil]].
 * Private params are remixed or converted for XML serialization with [[DDM]].
 */
case class DatasetMetadata(identifiers: Option[Seq[SchemedValue]] = None,
                           languageOfDescription: Option[SchemedKeyValue] = None,
                           titles: Option[Seq[String]] = None,
                           alternativeTitles: Option[Seq[String]] = None,
                           descriptions: Option[Seq[String]] = None,
                           creators: Option[Seq[Author]] = None,
                           contributors: Option[Seq[Author]] = None,
                           audiences: Option[Seq[SchemedKeyValue]] = None,
                           subjects: Option[Seq[SchemedKeyValue]] = None,
                           private val alternativeIdentifiers: Option[Seq[SchemedValue]] = None,
                           private val relations: Option[Seq[RelationType]] = None,
                           languagesOfFiles: Option[Seq[SchemedKeyValue]] = None,
                           private val dates: Option[Seq[Date]] = None,
                           sources: Option[Seq[String]] = None,
                           instructionsForReuse: Option[Seq[String]] = None,
                           publishers: Option[Seq[String]] = None,
                           accessRights: Option[AccessRights] = None,
                           license: Option[SchemedValue] = None,
                           types: Option[Seq[SchemedValue]] = None,
                           formats: Option[Seq[SchemedValue]] = None,
                           temporalCoverages: Option[Seq[SchemedKeyValue]] = None,
                           spatialPoints: Option[Seq[SpatialPoint]] = None,
                           spatialBoxes: Option[Seq[SpatialBox]] = None,
                           spatialCoverages: Option[Seq[SchemedKeyValue]] = None,
                           messageForDataManager: Option[String] = None,
                           private val privacySensitiveDataPresent: PrivacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified,
                           acceptDepositAgreement: Boolean = false,
                          ) {
  // lazy so missing values are only reported when converting to XML
  lazy val hasPrivacySensitiveData: Try[Boolean] = privacySensitiveDataPresent match {
    case PrivacySensitiveDataPresent.yes => Success(true)
    case PrivacySensitiveDataPresent.no => Success(false)
    case PrivacySensitiveDataPresent.unspecified => Failure(missingValue("PrivacySensitiveDataPresent"))
  }

  lazy val depositAgreementAccepted: Try[Unit] = if (acceptDepositAgreement) Success(())
                                                 else Failure(missingValue("AcceptDepositAgreement"))

  // not lazy so not allowed duplicates throw exceptions when parsing json, like other unknown fields
  val (datesCreated, datesAvailable, otherDates) = dates
    .getOrElse(Seq.empty)
    .map(_.toDayLevel)
    .separate

  lazy val authors: Seq[Author] = (contributors.toSeq ++ creators.toSeq).flatten

  lazy val allRelations: Seq[RelationType] = relations.getOrElse(Seq()) ++
    alternativeIdentifiers.getOrElse(Seq())
      .map(i => RelatedIdentifier(i.scheme, i.value, Option(RelationQualifier.isFormatOf)))

  //// doi
  lazy val doi: Option[String] = identifiers.flatMap(_.collectFirst {
    case SchemedValue(Some(`doiScheme`), Some(value)) => value
  })

  def setDoi(value: String): DatasetMetadata = {
    val ids = identifiers.getOrElse(Seq.empty).filterNot(_.scheme.contains(doiScheme))
    this.copy(identifiers = Some(ids :+ SchemedValue(doiScheme, value)))
  }
}

object DatasetMetadata {
  def apply(input: JsonInput): Try[DatasetMetadata] = input.deserialize[DatasetMetadata]

  val doiScheme = "id-type:DOI"

  def missingValue(label: String): InvalidDocumentException = {
    InvalidDocumentException("DatasetMetadata", new Exception(s"Please set $label"))
  }
}

