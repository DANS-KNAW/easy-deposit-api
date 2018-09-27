package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ SchemedKeyValue, SchemedValue, _ }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson

case class Author(titles: Option[String] = None,
                  initials: Option[String] = None,
                  insertions: Option[String] = None,
                  surname: Option[String] = None,
                  role: Option[SchemedKeyValue[String]] = None,
                  ids: Option[Seq[SchemedValue[String]]] = None,
                  organization: Option[String] = None,
                 ) {
  private val hasMandatory: Boolean = organization.isProvided || (surname.isProvided && initials.isProvided)
  private val hasRedundant: Boolean = surname.isEmpty && (titles.isProvided || insertions.isProvided)
  private val incompleteMsg = "needs one of (organisation | surname and initials)"
  private val redundantMsg = "without surname should have neither titles nor insertions"
  require(hasMandatory, buildMsg(incompleteMsg))
  require(!hasRedundant, buildMsg(redundantMsg))

  private def buildMsg(s: String) = s"Author $s; got: ${ toJson(this) }"

  def isRightsHolder: Boolean = role.exists(_.key == "RightsHolder")


  override def toString: String = { // TODO ID's when DatasetXml implements ID's for Author fields
    val name = Seq(titles, initials, insertions, surname)
      .filter(_.isProvided)
      .map(_.getOrElse(""))
      .mkString(" ")
    (surname.isProvided, organization.isProvided) match {
      case (true, true) => s"$name; ${ organization.getOrElse("") }"
      case (false, true) => organization.getOrElse("")
      case (true, false) => name
      case (false, false) => throw new Exception(buildMsg(incompleteMsg)) // only with wrong requires
    }
  }
}