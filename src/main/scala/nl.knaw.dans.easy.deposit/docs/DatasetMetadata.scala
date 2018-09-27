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
import nl.knaw.dans.easy.deposit.docs.dm.PrivacySensitiveDataPresent.PrivacySensitiveDataPresent
import nl.knaw.dans.easy.deposit.docs.dm._
import org.json4s.JsonInput

import scala.util.{ Failure, Success, Try }

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

  lazy val privacyBoolean: Try[Boolean] = privacySensitiveDataPresent match {
    case PrivacySensitiveDataPresent.yes => Success(true)
    case PrivacySensitiveDataPresent.no => Success(false)
    case PrivacySensitiveDataPresent.unspecified => Failure(missingValue("PrivacySensitiveDataPresent"))
  }

  lazy val licenceAccepted: Try[Unit] = if (acceptLicenseAgreement) Success(())
                                        else Failure(missingValue("AcceptLicenseAgreement"))

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

  case class QualifiedSchemedValue[S, Q](scheme: Option[S],
                                         value: String,
                                         qualifier: Q
                                        ) extends RequiresNonEmpty {
    requireNonEmptyString(value, "value")
  }

  case class SchemedValue[S](scheme: S,
                             value: String,
                            ) extends RequiresNonEmpty {
    requireNonEmptyString(scheme, "scheme")
    requireNonEmptyString(value, "value")
  }

  case class PossiblySchemedValue[S](scheme: Option[S],
                                     value: String,
                                    ) extends RequiresNonEmpty {
    requireNonEmptyString(value, "value")
  }

  case class SchemedKeyValue[S](scheme: S,
                                key: String,
                                value: String,
                               ) extends RequiresNonEmpty {
    requireNonEmptyString(scheme, "scheme")
    requireNonEmptyString(value, "value")
    requireNonEmptyString(key, "key")
  }

  case class PossiblySchemedKeyValue[S](scheme: Option[S],
                                        key: Option[String],
                                        value: String,
                                       ) extends RequiresNonEmpty {
    requireNonEmptyString(value, "value")
  }
}

