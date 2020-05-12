package nl.knaw.dans.easy.deposit.properties

import java.util.UUID

import nl.knaw.dans.easy.deposit.properties.DepositProperties.SubmittedProperties

import scala.util.Try

class CompoundDepositProperties(file: DepositProperties,
                                service: DepositProperties,
                               ) extends DepositProperties {

  override def depositId: UUID = {
    val _ = service.depositId
    file.depositId
  }

  override def getSubmittedProperties: Try[SubmittedProperties] = {
    for {
      _ <- service.getSubmittedProperties
      props <- file.getSubmittedProperties
    } yield props
  }
}
