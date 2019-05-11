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

import nl.knaw.dans.easy.deposit.docs.dm.AccessRights.AccessRights
import nl.knaw.dans.easy.deposit.docs.dm.DateScheme.W3CDTF
import nl.knaw.dans.easy.deposit.docs.dm.PrivacySensitiveDataPresent.PrivacySensitiveDataPresent
import nl.knaw.dans.easy.deposit.docs.dm._

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
                              subjects: Option[Seq[SchemedKeyValue]] = None,
                              alternativeIdentifiers: Option[Seq[SchemedValue]] = None,
                              relations: Option[Seq[RelationType]] = None,
                              languagesOfFiles: Option[Seq[SchemedKeyValue]] = None,
                              dates: Option[Seq[Date]] = Some(Seq(
                                Date(scheme = Some(W3CDTF.toString), value = Some("2018"), Some(DateQualifier.created)),
                                Date(scheme = Some(W3CDTF.toString), value = Some("2018"), Some(DateQualifier.available))
                              )),
                              sources: Option[Seq[String]] = None,
                              instructionsForReuse: Option[Seq[String]] = None,
                              publishers: Option[Seq[String]] = None,
                              accessRights: Option[AccessRights] = Some(AccessRights.open),
                              license: Option[SchemedValue] = None,
                              typesDcmi: Option[Seq[String]] = None,
                              types: Option[Seq[SchemedValue]] = None,
                              formats: Option[Seq[SchemedValue]] = None,
                              temporalCoverages: Option[Seq[SchemedKeyValue]] = None,
                              spatialPoints: Option[Seq[SpatialPoint]] = None,
                              spatialBoxes: Option[Seq[SpatialBox]] = None,
                              spatialCoverages: Option[Seq[SchemedKeyValue]] = None,
                              messageForDataManager: Option[String] = None,
                              privacySensitiveDataPresent: PrivacySensitiveDataPresent = PrivacySensitiveDataPresent.unspecified,
                              acceptDepositAgreement: Boolean = false,
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
    acceptDepositAgreement,
  )
