package nl.knaw.dans.easy.deposit.docs

import org.joda.time.DateTime

import scala.util.Try
import scala.xml.Elem

object DatasetXml {
  def apply(dateSubmitted: DateTime, dm: DatasetMetadata): Try[Elem] = {
    for {
      _ <- dm.licenceAccepted
      privacy <- dm.privacyBoolean
    } yield
      <stub/>
  }
}
