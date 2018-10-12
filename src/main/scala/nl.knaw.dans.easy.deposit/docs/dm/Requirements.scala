package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson

import scala.util.Try

trait Requirements {
  def requireNonEmptyString[T](value: T): Unit = {
    value match {
      case str: String =>
        require(str.trim.nonEmpty, buildMsg("Empty string for a mandatory field"))
      case _ =>
    }
  }

  // using doubles as arguments could change precision in the XML output by adding ".0"
  protected def requireDouble(value: String): Unit = {
    require(Try(value.toDouble).isSuccess, buildMsg(s"Invalid number [$value]"))
  }

  protected def buildMsg(msg: String) = s"$msg; got ${ toJson(this) } ${ this.getClass.getSimpleName }"
}
