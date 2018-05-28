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
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.{ DateQualifier, dateSubmitted }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.PrivacySensitiveDataPresent.{ PrivacySensitiveDataPresent, unspecified }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._
import nl.knaw.dans.easy.deposit.docs.Json.{ InvalidDocumentException, RichJsonInput }
import org.joda.time.DateTime
import org.json4s.JsonInput

import scala.util.{ Failure, Success, Try }
import scala.xml.Elem

case class DatasetMetadata(doi: Option[String] = None,
                           languageOfDescription: Option[String] = None,
                           titles: Option[Seq[String]] = None,
                           alternativeTitles: Option[Seq[String]] = None,
                           descriptions: Option[Seq[String]] = None,
                           creators: Option[Seq[Author]] = None,
                           contributors: Option[Seq[Author]] = None,
                           dateCreated: Option[String] = None,
                           audiences: Option[Seq[String]] = None,
                           subjects: Option[Seq[String]] = None,
                           identifiers: Option[Seq[SchemedValue]] = None,
                           relations: Option[Seq[Relation]] = None,
                           languagesOfFilesIso639: Option[Seq[String]] = None,
                           languagesOfFiles: Option[Seq[String]] = None,
                           dates: Option[Seq[QualifiedSchemedValue]] = None,
                           sources: Option[Seq[String]] = None,
                           instructionsForReuse: Option[Seq[String]] = None,
                           rightsHolders: Option[Seq[String]] = None,
                           publishers: Option[Seq[String]] = None,
                           accessRights: Option[AccessRights] = None,
                           license: Option[String] = None,
                           dateAvailable: Option[String] = None,
                           typesDcmi: Option[Seq[String]] = None,
                           types: Option[Seq[String]] = None,
                           formatsMediaType: Option[Seq[String]] = None,
                           formats: Option[Seq[String]] = None,
                           temporalCoverages: Option[Seq[PossiblySchemedKeyValue]] = None,
                           spatialPoints: Option[Seq[SpatialPoint]] = None,
                           spatialBoxes: Option[Seq[SpatialBox]] = None,
                           spatialCoverages: Option[Seq[PossiblySchemedKeyValue]] = None,
                           messageForDataManager: Option[String] = None,
                           privacySensitiveDataPresent: PrivacySensitiveDataPresent = unspecified,
                           acceptLicenseAgreement: Boolean = false,
                          ) {

  private lazy val submitDate: Option[String] = {
    dates.flatMap(_.find(_.qualifier == dateSubmitted)).map(_.value)
  }

  lazy val xml: Try[Elem] = Success(<stub/>) // TODO

  def setDateSubmitted(): Try[DatasetMetadata] = {
    if (submitDate.isDefined)
      Failure(new Exception("dateSubmitted should not be present"))
    else {
      val now = DateTime.now().toString("yyyy-MM-dd")
      val submitted = QualifiedSchemedValue(Some("dcterms:W3CDTF"), now, dateSubmitted)
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
    val open: AccessCategory = Value("open")
    val openForRegisteredUsers: AccessCategory = Value("open_for_registered_users")
    val restrictedGroup: AccessCategory = Value("restricted_group")
    val restrictedRequest: AccessCategory = Value("restricted_request")
    val otherAccess: AccessCategory = Value("other_access")
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
  }

  case class AccessRights(category: AccessCategory,
                          group: String,
                         )

  case class QualifiedSchemedValue(scheme: Option[String],
                                   value: String,
                                   qualifier: DateQualifier)

  case class Author(titles: Option[String] = None,
                    initials: Option[String] = None,
                    insertions: Option[String] = None,
                    surname: Option[String] = None,
                    role: Option[SchemedKeyValue] = None,
                    ids: Option[Seq[SchemedValue]] = None,
                    organization: Option[String] = None,
                   ) {
    def isValid: Boolean = {
      organization.isDefined ||
        (surname.isDefined && initials.isDefined)
    }
  }

  case class Date(scheme: Option[String] = None,
                  date: Option[String] = None,
                 )

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

  case class SchemedValue(scheme: String,
                          value: String,
                         )

  case class PossiblySchemedValue(scheme: Option[String],
                                  value: String,
                                 )

  case class SchemedKeyValue(scheme: String,
                             key: String,
                             value: String,
                            )

  case class PossiblySchemedKeyValue(scheme: Option[String],
                                     key: Option[String],
                                     value: String,
                                    )

  case class Identifier(scheme: String,
                        identifier: String,
                       )

  case class Relation(qualifier: Option[String] = None,
                      url: Option[String] = None,
                      title: Option[String] = None,
                     )
}

