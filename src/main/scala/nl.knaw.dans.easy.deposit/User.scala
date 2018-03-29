package nl.knaw.dans.easy.deposit

case class User(userName: String, firstName: Option[String] = None, prefix: Option[String] = None, lastName: String, groups: Option[Seq[String]] = None)
object User {
  def apply(attributes: Map[String, Seq[String]]): User = {
    // For possible attribute keys see: https://github.com/DANS-KNAW/dans.easy-test-users/blob/master/templates
    new User(

      // mandatory: https://github.com/DANS-KNAW/dans.easy-ldap-dir/blob/f17c391/files/easy-schema.ldif#L83-L84
      userName = attributes.getOrElse("uid", Seq.empty).headOption.getOrElse(""),

      firstName = attributes.getOrElse("cn", Seq.empty).headOption,

      // https://github.com/DANS-KNAW/easy-app/blob/b41e9e93e35f97af00c48d0515b09cc57bc5ba6c/lib-deprecated/dans-ldap/src/main/java/nl/knaw/dans/common/ldap/management/DANSSchema.java#L33-L34
      prefix = attributes.get("dansPrefixes").map(s => s.mkString(" ")),

      lastName = attributes.getOrElse("sn", Seq.empty).headOption.getOrElse(""),

      groups = attributes.get("easyGroups")
    )
  }
}