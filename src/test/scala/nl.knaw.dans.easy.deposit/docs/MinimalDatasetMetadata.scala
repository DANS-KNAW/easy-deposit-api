package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ PossiblySchemedKeyValue, PossiblySchemedValue, SchemedKeyValue, SchemedValue }
import nl.knaw.dans.easy.deposit.docs.dm.DateScheme.W3CDTF
import nl.knaw.dans.easy.deposit.docs.dm.PrivacySensitiveDataPresent.PrivacySensitiveDataPresent
import nl.knaw.dans.easy.deposit.docs.dm.{ Date, _ }

class MinimalDatasetMetadata(
                              identifiers: Option[Seq[SchemedValue]] = Some(Seq(
                                SchemedValue(scheme = "id-type:DOI", value = "mocked-DOI")
                              )),
                              languageOfDescription: Option[SchemedKeyValue] = None,
                              titles: Option[Seq[String]] = Some(Seq(
                                "Lorum ipsum"
                              )),
                              alternativeTitles: Option[Seq[String]] = None,
                              descriptions: Option[Seq[String]] = Some(Seq(
                                "dolor"
                              )),
                              creators: Option[Seq[Author]] = Some(Seq(
                                Author(initials = Some("B.A.R."), surname = Some("Foo"))
                              )),
                              contributors: Option[Seq[Author]] = None,
                              audiences: Option[Seq[SchemedKeyValue]] = Some(Seq(
                                SchemedKeyValue(scheme = "blabla", key = "D35200", value = "some audience")
                              )),
                              subjects: Option[Seq[PossiblySchemedKeyValue]] = None,
                              alternativeIdentifiers: Option[Seq[SchemedValue]] = None,
                              relations: Option[Seq[RelationType]] = None,
                              languagesOfFiles: Option[Seq[PossiblySchemedKeyValue]] = None,
                              dates: Option[Seq[Date]] = Some(Seq(
                                Date(scheme = Some(W3CDTF.toString), value = "2018", DateQualifier.created),
                                Date(scheme = Some(W3CDTF.toString), value = "2018", DateQualifier.available)
                              )),
                              sources: Option[Seq[String]] = None,
                              instructionsForReuse: Option[Seq[String]] = None,
                              publishers: Option[Seq[String]] = None,
                              accessRights: Option[AccessRights] = Some(
                                AccessRights(category = AccessCategory.open, group = None)
                              ),
                              license: Option[String] = None,
                              typesDcmi: Option[Seq[String]] = None,
                              types: Option[Seq[PossiblySchemedValue]] = None,
                              formats: Option[Seq[PossiblySchemedValue]] = None,
                              temporalCoverages: Option[Seq[PossiblySchemedKeyValue]] = None,
                              spatialPoints: Option[Seq[SpatialPoint]] = None,
                              spatialBoxes: Option[Seq[SpatialBox]] = None,
                              spatialCoverages: Option[Seq[PossiblySchemedKeyValue]] = None,
                              messageForDataManager: Option[String] = None,
                              privacySensitiveDataPresent: PrivacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified,
                              acceptLicenseAgreement: Boolean = false,
                            )
  extends DatasetMetadata(
    identifiers,
    languageOfDescription,
    titles,
    alternativeTitles,
    descriptions,
    creators,
    contributors,
    audiences,
    subjects,
    alternativeIdentifiers,
    relations,
    languagesOfFiles,
    dates,
    sources,
    instructionsForReuse,
    publishers,
    accessRights,
    license,
    typesDcmi,
    types,
    formats,
    temporalCoverages,
    spatialPoints,
    spatialBoxes,
    spatialCoverages,
    messageForDataManager,
    privacySensitiveDataPresent,
    acceptLicenseAgreement,
  )
