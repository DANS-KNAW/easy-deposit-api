package nl.knaw.dans.easy.deposit.docs

import org.joda.time.DateTime

import scala.util.{ Failure, Success, Try }
import scala.xml.Elem

object AgreementsXml {
  def apply(userId: String, dateSubmitted: DateTime, dm: DatasetMetadata): Try[Elem] = {
    for {
      _ <- dm.licenceAccepted
      privacy <- dm.privacyBoolean
    } yield
      <agr:agreements
          xmlns:agr="http://easy.dans.knaw.nl/schemas/bag/metadata/agreements/"
          xmlns:dcterms="http://purl.org/dc/terms/"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/agreements/ agreements.xsd">
        <agr:licenseAgreement>
          <agr:depositorId>{userId}</agr:depositorId>
          <dateAccepted>{dateSubmitted}</dateAccepted>
          <agr:licenseAgreementAccepted>{dm.acceptLicenseAgreement}</agr:licenseAgreementAccepted>
        </agr:licenseAgreement>
        <agr:personalDataStatement>
          <agr:signerId>{userId}</agr:signerId>
          <agr:dateSigned>{dateSubmitted}</agr:dateSigned>
          <agr:containsPrivacySensitiveData>{privacy}</agr:containsPrivacySensitiveData>
        </agr:personalDataStatement>
      </agr:agreements>
  }
}
