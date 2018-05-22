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
import nl.knaw.dans.easy.deposit.docs.Json.InvalidDocumentException
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

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
                           dates: Option[Seq[QualifiedDate]] = None,
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
                           archisNrs: Option[Seq[String]] = None,
                           subjectsAbrComplex: Option[Seq[String]] = None,
                           temporalCoveragesAbr: Option[Seq[String]] = None,
                           temporalCoverages: Option[Seq[String]] = None,
                           spatialPoints: Option[Seq[SpatialPoint]] = None,
                           spatialBoxes: Option[Seq[SpatialBox]] = None,
                           spatialCoverageIso3166: Option[Seq[SchemedValue]] = None,
                           spatialCoverages: Option[Seq[String]] = None,
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
      val now = DateTimeFormat.forPattern("yyyy-MM-dd").print(DateTime.now())
      val submitted = QualifiedDate(None, now, dateSubmitted)
      val newDates = dates match {
        case None => Seq(submitted)
        case Some(d) => Seq(submitted) ++ d
      }
      Success(copy(dates = Some(newDates)))
    }
  }

  def agreements(userId: String): Try[Elem] = {
    for {
      _ <- verify("AcceptLicenseAgreement", acceptLicenseAgreement)
      _ <- verify("PrivacySensitiveDataPresent", privacySensitiveDataPresent != unspecified)
      date <- Try { submitDate.get }
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
          <agr:containsPrivacySensitiveData>{privacySensitiveDataPresent match {
            case PrivacySensitiveDataPresent.no => false
            case PrivacySensitiveDataPresent.yes => true
            case _ => // should never happen because of verify call
          }}</agr:containsPrivacySensitiveData>
        </agr:personalDataStatement>
      </agr:agreements>
  }

  private def verify(label: String, condition: Boolean) = {
    if (condition) Success(())
    else Failure(InvalidDocumentException(s"Please set $label in DatasetMetadata", new IllegalArgumentException()))
  }
}

object DatasetMetadata {

  object PrivacySensitiveDataPresent extends Enumeration {
    type PrivacySensitiveDataPresent = Value
    val yes, no, unspecified = Value
  }

  object AccessCategory extends Enumeration {
    type AccessCategory = Value
    val open, open_for_registered_users, restricted_group, restricted_request, other_access = Value
  }

  object DateQualifier extends Enumeration {
    type DateQualifier = Value
    val created, available, date, dateAccepted, dateCopyrighted, dateSubmitted, issued, modified, valid = Value
  }

  case class AccessRights(category: AccessCategory,
                          group: String,
                         )

  case class QualifiedDate(scheme: Option[String],
                           value: String,
                           qualifier: DateQualifier)

  case class Author(titles: Option[String] = None,
                    initials: Option[String] = None,
                    insertions: Option[String] = None,
                    surname: Option[String] = None,
                    role: Option[String] = None,
                    ids: Option[Seq[SchemedValue]] = None,
                    organization: Option[String] = None,
                   )

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

  case class Identifier(scheme: String,
                        identifier: String,
                       )

  case class Relation(qualifier: Option[String] = None,
                      url: Option[String] = None,
                      title: Option[String] = None,
                     )
}

