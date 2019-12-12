package nl.knaw.dans.easy.deposit.docs

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
object AgreementData {
  def apply(userData: UserData, dm: DatasetMetadata): AgreementData = {
    // the ui and ingest-flow validate, so just prevent exceptions here on absent values
    new AgreementData(depositor = AgreementUser(userData),
      doi = dm.doi.getOrElse(""),
      title= dm.titles.getOrElse(Seq.empty).headOption.getOrElse(""),
      dateSubmitted = DateTime.now.toString(ISODateTimeFormat.date()), // TODO from deposit properties?
      dateAvailable = dm.datesAvailable.map(_.toDayLevel.toString).getOrElse("TODAY"),
      accessCategory = dm.accessRights.map(_.toString).getOrElse(""),
      license = dm.license.flatMap(_.value).getOrElse(""),
    )
  }
}
