package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.JsonUtil

trait OptionalQualifier[T] {
  val qualifier: Option[T]

  lazy val qualifierOrThrow: String = qualifier.map(_.toString)
    .getOrElse(throw new IllegalArgumentException(
      s"no qualifier for ${getClass.getSimpleName}: ${JsonUtil.toJson(this)}"
    ))
}
