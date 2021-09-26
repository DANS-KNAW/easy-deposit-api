/*
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

import nl.knaw.dans.easy.deposit.docs.dm.DateQualifier
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

case class AgreementData(depositor: AgreementUser,
                         doi: String,
                         title: String,
                         dateSubmitted: String,
                         dateAvailable: String,
                         accessCategory: String,
                         license: String,
                         sample: Boolean = false,
                         agreementVersion: String = "4.0",
                         agreementLanguage: String = "EN",
                        )
object AgreementData extends DebugEnhancedLogging {
  def apply(userData: UserData, dm: DatasetMetadata): AgreementData = {
    // the ui and ingest-flow validate, so just prevent exceptions on absent values
    new AgreementData(depositor = AgreementUser(userData),
      doi = dm.doi.getOrElse(""),
      title = dm.titles.getOrElse(Seq.empty).headOption.getOrElse(""),
      accessCategory = dm.accessRights.map(_.toString).getOrElse(""),
      license = dm.license.flatMap(_.value).getOrElse(""),
      dateAvailable = dm.datesAvailable.map(_.value.getOrElse("")).getOrElse(""),
      dateSubmitted = dm.otherDates
        .find(_.qualifier.contains(DateQualifier.dateSubmitted)).flatMap(_.value)
        .getOrElse(DateTime.now.toString(ISODateTimeFormat.date()))
    )
  }
}
