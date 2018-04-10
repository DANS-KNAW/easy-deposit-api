package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.AccessCategory.AccessCategory
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.PrivacySensitiveDataPresent.{ PrivacySensitiveDataPresent, unspecified }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._

case class DatasetMetadata(doi: Option[String] = None,
                           languageOfDescription: Option[String] = None,
                           titles: Seq[String] = Seq.empty,
                           alternativeTitles: Seq[String] = Seq.empty,
                           descriptions: Seq[String] = Seq.empty,
                           creators: Seq[Author] = Seq.empty,
                           contributors: Seq[Author] = Seq.empty,
                           dateCreated: Option[String] = None,
                           audiences: Seq[String] = Seq.empty,
                           subjects: Seq[String] = Seq.empty,
                           identifiers: Seq[SchemedValue] = Seq.empty,
                           relations: Seq[Relation] = Seq.empty,
                           languagesOfFilesIso639: Seq[String] = Seq.empty,
                           languagesOfFiles: Seq[String] = Seq.empty,
                           datesIso8601: Seq[SchemedValue] = Seq.empty,
                           dates: Seq[SchemedValue] = Seq.empty,
                           sources: Seq[String] = Seq.empty,
                           instructionsForReuse: Seq[String] = Seq.empty,
                           rightsHolders: Seq[String] = Seq.empty,
                           publishers: Seq[String] = Seq.empty,
                           accessRights: Option[AccessRights] = None,
                           license: Option[String],
                           dateAvailable: Option[String],
                           typesDcmi: Seq[String] = Seq.empty,
                           types: Seq[String] = Seq.empty,
                           formatsMediaType: Seq[String] = Seq.empty,
                           formats: Seq[String] = Seq.empty,
                           archisNrs: Seq[String] = Seq.empty,
                           subjectsAbrComplex: Seq[String] = Seq.empty,
                           temporalCoveragesAbr: Seq[String] = Seq.empty,
                           extraClarinMetadataPresent: Boolean = false,
                           temporalCoverages: Seq[String] = Seq.empty,
                           spatialPoints: Seq[SpatialPoint] = Seq.empty,
                           spatialBoxes: Seq[SpatialBox] = Seq.empty,
                           spatialCoverageIso3166: Seq[SchemedValue] = Seq.empty,
                           spatialCoverages: Seq[String] = Seq.empty,
                           messageForDataManager: Option[String],
                           privacySensitiveDataPresent: PrivacySensitiveDataPresent = unspecified,
                           acceptLicenseAgreement: Boolean = false,
                          )
object DatasetMetadata {

  object PrivacySensitiveDataPresent extends Enumeration {
    type PrivacySensitiveDataPresent = Value
    val yes, no, unspecified = Value
  }

  object AccessCategory extends Enumeration {
    type AccessCategory = Value
    val open, open_for_registered_users, restricted_group, restricted_request, other_access = Value
  }

  case class AccessRights(category: AccessCategory,
                          group: String,
                         )

  case class Author(titles: Option[String] = None,
                    initials: Option[String] = None,
                    insertions: Option[String] = None,
                    surname: Option[String] = None,
                    role: Option[String] = None,
                    ids: Seq[SchemedValue] = Seq.empty,
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

