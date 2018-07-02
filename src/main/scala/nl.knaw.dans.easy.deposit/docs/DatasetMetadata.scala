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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.AccessCategory.AccessCategory
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.DateQualifier
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.PrivacySensitiveDataPresent.PrivacySensitiveDataPresent
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.RelationQualifier.RelationQualifier
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.{ InvalidDocumentException, RichJsonInput, toJson }
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonInput

import scala.util.{ Failure, Success, Try }
import scala.xml.Elem

case class DatasetMetadata(identifiers: Option[Seq[SchemedValue[String]]] = None,
                           languageOfDescription: Option[SchemedKeyValue[String]] = None,
                           titles: Option[Seq[String]] = None,
                           alternativeTitles: Option[Seq[String]] = None,
                           descriptions: Option[Seq[String]] = None,
                           creators: Option[Seq[Author]] = None,
                           contributors: Option[Seq[Author]] = None,
                           audiences: Option[Seq[SchemedKeyValue[String]]] = None,
                           subjects: Option[Seq[PossiblySchemedKeyValue[String]]] = None,
                           alternativeIdentifiers: Option[Seq[SchemedValue[String]]] = None,
                           relations: Option[Seq[RelationType]] = None,
                           languagesOfFiles: Option[Seq[PossiblySchemedKeyValue[String]]] = None,
                           dates: Option[Seq[Date]] = None,
                           sources: Option[Seq[String]] = None,
                           instructionsForReuse: Option[Seq[String]] = None,
                           publishers: Option[Seq[String]] = None,
                           accessRights: Option[AccessRights] = None,
                           license: Option[String] = None,
                           typesDcmi: Option[Seq[String]] = None,
                           types: Option[Seq[PossiblySchemedValue[String]]] = None,
                           formats: Option[Seq[PossiblySchemedValue[String]]] = None,
                           temporalCoverages: Option[Seq[PossiblySchemedKeyValue[String]]] = None,
                           spatialPoints: Option[Seq[SpatialPoint]] = None,
                           spatialBoxes: Option[Seq[SpatialBox]] = None,
                           spatialCoverages: Option[Seq[PossiblySchemedKeyValue[String]]] = None,
                           messageForDataManager: Option[String] = None,
                           privacySensitiveDataPresent: PrivacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified,
                           acceptLicenseAgreement: Boolean = false,
                          ) {
  lazy val doi: Option[String] = identifiers.flatMap(_.collectFirst {
    case SchemedValue(`doiScheme`, value) => value
  })

  def setDoi(value: String): DatasetMetadata = {
    val ids = identifiers.getOrElse(Seq.empty).filter(_.scheme == doiScheme)
    this.copy(identifiers = Some(ids :+ SchemedValue(doiScheme, value)))
  }

  private lazy val submitDate: Option[String] = {
    dates.flatMap(_.find(_.qualifier == DateQualifier.dateSubmitted)).map(_.value)
  }

  lazy val xml: Try[Elem] = Success(<stub/>) // TODO

  def setDateSubmitted(): Try[DatasetMetadata] = {
    if (submitDate.isDefined)
      Failure(new Exception("dateSubmitted should not be present"))
    else {
      val submitted = Date(DateTime.now(), DateQualifier.dateSubmitted)
      val newDates = submitted +: dates.getOrElse(Seq.empty)
      Success(copy(dates = Some(newDates)))
    }
  }

  def agreements(userId: String): Try[Elem] = {
    for {
      _ <- if (acceptLicenseAgreement) Success(())
           else Failure(missingValue("AcceptLicenseAgreement"))
      privacy <- toBoolean(privacySensitiveDataPresent)
      date = submitDate.getOrElse(throw new IllegalArgumentException("no submitDate"))
    } yield
      <agr:agreements
          xmlns:agr="http://easy.dans.knaw.nl/schemas/bag/metadata/agreements/"
          xmlns:dcterms="http://purl.org/dc/terms/"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/agreements/ agreements.xsd">
        <agr:licenseAgreement>
          <agr:depositorId>{userId}</agr:depositorId>
          <dateAccepted>{date}</dateAccepted>
          <agr:licenseAgreementAccepted>{acceptLicenseAgreement}</agr:licenseAgreementAccepted>
        </agr:licenseAgreement>
        <agr:personalDataStatement>
          <agr:signerId>{userId}</agr:signerId>
          <agr:dateSigned>{date}</agr:dateSigned>
          <agr:containsPrivacySensitiveData>{privacy}</agr:containsPrivacySensitiveData>
        </agr:personalDataStatement>
      </agr:agreements>
  }
}

object DatasetMetadata {
  def apply(input: JsonInput): Try[DatasetMetadata] = input.deserialize[DatasetMetadata]

  private val doiScheme = "id-type:DOI"
  private val dateScheme = "dcterms:W3CDTF"

  type Date = QualifiedSchemedValue[String, DateQualifier]

  def Date(value: DateTime,
           qualifier: DateQualifier
          ): Date = {
    QualifiedSchemedValue[String, DateQualifier](
      Some(dateScheme),
      value.toString(ISODateTimeFormat.date()),
      qualifier
    )
  }

  private def missingValue(label: String) = {
    InvalidDocumentException(s"Please set $label in DatasetMetadata")
  }

  private def toBoolean(privacySensitiveDataPresent: PrivacySensitiveDataPresent): Try[Boolean] = {
    privacySensitiveDataPresent match {
      case PrivacySensitiveDataPresent.yes => Success(true)
      case PrivacySensitiveDataPresent.no => Success(false)
      case PrivacySensitiveDataPresent.unspecified => Failure(missingValue("PrivacySensitiveDataPresent"))
    }
  }

  object PrivacySensitiveDataPresent extends Enumeration {
    type PrivacySensitiveDataPresent = Value
    val yes, no, unspecified = PrivacySensitiveDataPresent.Value
  }

  object AccessCategory extends Enumeration {
    type AccessCategory = Value
    val open: AccessCategory = Value("OPEN_ACCESS")
    val openForRegisteredUsers: AccessCategory = Value("OPEN_ACCESS_FOR_REGISTERED_USERS")
    val restrictedGroup: AccessCategory = Value("GROUP_ACCESS")
    val restrictedRequest: AccessCategory = Value("REQUEST_PERMISSION")
    val otherAccess: AccessCategory = Value("NO_ACCESS")
  }

  object DateQualifier extends Enumeration {
    type DateQualifier = Value
    val created: DateQualifier = Value("dcterms:created")
    val available: DateQualifier = Value("dcterms:available")
    val date: DateQualifier = Value("dcterms:date")
    val dateAccepted: DateQualifier = Value("dcterms:dateAccepted")
    val dateCopyrighted: DateQualifier = Value("dcterms:dateCopyrighted")
    val dateSubmitted: DateQualifier = Value("dcterms:dateSubmitted")
    val issued: DateQualifier = Value("dcterms:issued")
    val modified: DateQualifier = Value("dcterms:modified")
    val valid: DateQualifier = Value("dcterms:valid")
  }

  object RelationQualifier extends Enumeration {
    type RelationQualifier = Value
    val hasFormat: RelationQualifier = Value("dcterms:hasFormat")
    val hasPart: RelationQualifier = Value("dcterms:hasPart")
    val hasVersion: RelationQualifier = Value("dcterms:hasVersion")
    val isFormatOf: RelationQualifier = Value("dcterms:isFormatOf")
    val isPartOf: RelationQualifier = Value("dcterms:isPartOf")
    val isReferencedBy: RelationQualifier = Value("dcterms:isReferencedBy")
    val isReplacedBy: RelationQualifier = Value("dcterms:isReplacedBy")
    val isRequiredBy: RelationQualifier = Value("dcterms:isRequiredBy")
    val isVersionOf: RelationQualifier = Value("dcterms:isVersionOf")
    val references: RelationQualifier = Value("dcterms:references")
    val relation: RelationQualifier = Value("dcterms:relation")
    val replaces: RelationQualifier = Value("dcterms:replaces")
    val requires: RelationQualifier = Value("dcterms:requires")
    val conformsTo: RelationQualifier = Value("dcterms:conformsTo")
  }

  case class AccessRights(category: AccessCategory,
                          group: Option[String],
                         )

  case class Author(titles: Option[String],
                    initials: Option[String],
                    insertions: Option[String],
                    surname: Option[String],
                    role: Option[SchemedKeyValue[String]],
                    ids: Option[Seq[SchemedValue[String]]],
                    organization: Option[String],
                   ) {
    require(isValid, s"Author needs one of (organisation | surname and initials) got: ${ toJson(this) }")

    def isValid: Boolean = {
      organization.isDefined ||
        (surname.isDefined && initials.isDefined)
    }
  }

  case class SpatialPoint(scheme: String,
                          x: Int,
                          y: Int,
                         )

  case class SpatialBox(scheme: String,
                        north: Int,
                        east: Int,
                        south: Int,
                        west: Int,
                       )

  trait RelationType

  case class Relation(qualifier: RelationQualifier,
                      url: Option[String],
                      title: Option[String],
                     ) extends RelationType {
    require(isValid, "Relation needs at least one of (title | url) got: ${toJson(this)}")

    def isValid: Boolean = {
      title.isDefined || url.isDefined
    }
  }

  case class RelatedIdentifier(scheme: Option[String],
                               value: String,
                               qualifier: RelationQualifier) extends RelationType

  case class QualifiedSchemedValue[S, Q](scheme: Option[S],
                                         value: String,
                                         qualifier: Q)

  case class SchemedValue[S](scheme: S,
                             value: String,
                            )

  case class PossiblySchemedValue[S](scheme: Option[S],
                                     value: String,
                                    )

  case class SchemedKeyValue[S](scheme: S,
                                key: String,
                                value: String,
                               )

  case class PossiblySchemedKeyValue[S](scheme: Option[S],
                                        key: Option[String],
                                        value: String,
                                       )
}

