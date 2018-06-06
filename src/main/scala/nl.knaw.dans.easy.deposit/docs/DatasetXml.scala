package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.{ available, created }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ AccessRights, Author, QualifiedSchemedValue, SchemedKeyValue }

import scala.util.Try
import scala.xml.Elem

object DatasetXml {
  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    // TODO dc* namespaces
    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    >
      <ddm:profile>
        { requiredElems(dm.titles, "dc:title") }
        { requiredElems(dm.descriptions, "dc:description") }
        { requiredElems(dm.creators, "dcx-dai:creatorDetails") }
        { requiredElems(dm.dates.map(filter(_, created)), "ddm:created") }
        { requiredElems(dm.dates.map(filter(_, available)), "ddm:available") }
        { requiredElems(dm.audiences, "ddm:audience") }
        { requiredElems(dm.accessRights.map(Seq(_)), "ddm:accessRights") }
      </ddm:profile>
    </ddm:DDM>
  }

  private def elem[T](target: String)(source: T): Elem = (source match {
    case QualifiedSchemedValue(Some(scheme), value, _) => <key scheme={scheme.toString}>{value}</key>
    case QualifiedSchemedValue(None, value, _) => <key>{value}</key> // TODO so far just date-created/available
    case SchemedKeyValue(scheme, key, _) => <key scheme={scheme.toString}>{key}</key> // e.g. role, audience
    case a: AccessRights => <key>{a.category.toString}</key>
    case a: Author => <key>{authorDetails(a)}</key>
    case v => <key>{v}</key>
  }).copy(label = target)

  private def authorDetails(a: Author) =
    <dcx-dai:author>
      { optionalElem(a.titles, "dcx-dai:titles") }
      { optionalElem(a.initials, "dcx-dai:initials") }
      { optionalElem(a.insertions, "dcx-dai:insertions") }
      { optionalElem(a.surname, "dcx-dai:surname") }
      { optionalElem(a.role, "dcx-dai:role") }
      { a.organization.map(str => // TODO what is the source of the language?
          <dcx-dai:organization>
            <dcx-dai:name xml:lang="en">{str}</dcx-dai:name>
          </dcx-dai:organization>
        ).getOrElse(Seq.empty)
      }
    </dcx-dai:author>

  private def requiredElems[T](source: Option[Seq[T]], target: String): Seq[Elem] = {
    source.map(_.map(elem(target))).getOrElse(throwMandatory(target))
  }

  private def elems[T](source: Option[Seq[T]], target: String): Seq[Elem] = {
    source.map(_.map(elem(target))).getOrElse(Seq.empty)
  }

  private def optionalElem[T](source: Option[T], target: String): Seq[Elem] = {
    source.map(elem(target)).toSeq
  }

  private def filter[S, T](xs: Seq[QualifiedSchemedValue[S, T]],
                           q: T*
                          ): Seq[QualifiedSchemedValue[S, T]] = {
    xs.filter(x => q.contains(x.qualifier))
  }

  private def throwMandatory(tag: String) = {
    throw new IllegalArgumentException(s"no content for mandatory $tag")
  }
}
