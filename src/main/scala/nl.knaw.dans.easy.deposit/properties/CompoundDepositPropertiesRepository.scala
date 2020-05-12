package nl.knaw.dans.easy.deposit.properties

import java.util.UUID

import scala.util.Try

class CompoundDepositPropertiesRepository(file: FileDepositPropertiesRepository,
                                          service: ServiceDepositPropertiesRepository,
                                         ) extends DepositPropertiesRepository {

  override def load(depositId: UUID): Try[DepositProperties] = {
    for {
      serviceProps <- service.load(depositId)
      fileProps <- file.load(depositId)
    } yield new CompoundDepositProperties(fileProps, serviceProps)
  }
}
