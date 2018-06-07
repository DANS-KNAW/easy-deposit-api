package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.{ DateQualifier, available, created }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._

import scala.util.Try
import scala.xml.Elem

object DatasetXml {
  private type Lang = Option[SchemedKeyValue[String]]
  private val otherDates = DateQualifier.values.filter(d => d != created && d != available).toSeq

  def apply(dm: DatasetMetadata): Try[Elem] = Try { // TODO "dct:" or "dcterms:"
    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dct="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
    >
      <ddm:profile>
        { requiredElems(dm.titles, "dct:title", dm.languageOfDescription) }
        { requiredElems(dm.alternativeTitles, "dct:alternative", dm.languageOfDescription) }
        { requiredElems(dm.descriptions, "dc:description", dm.languageOfDescription) }
        { requiredElems(dm.creators, "dcx-dai:creatorDetails") }
        { requiredElems(dm.dates.map(filter(_, Seq(created))), "ddm:created") }
        { requiredElems(dm.dates.map(filter(_, Seq(available))), "ddm:available") }
        { requiredElems(dm.audiences, "ddm:audience") }
        { requiredElems(dm.accessRights.map(Seq(_)), "ddm:accessRights") }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { elems(dm.contributors, "dcx-dai:creatorDetails") }
        { elems(dm.dates.map(filter(_, otherDates)), "otherDate") }
        { optionalElem(dm.license, "dct:license") /* TODO xsi:type="dct:URI" not supported by json */ }
      </ddm:dcmiMetadata>
      <ddm:additional-xml>
      </ddm:additional-xml>
    </ddm:DDM>
  }

  // called by requiredElems, elems, optionalElem
  private def elem[T](target: String, lang: Lang)(source: T): Elem = {
    (source match {
      case a: Author => <key>{authorDetails(a, lang)}</key>
      case QualifiedSchemedValue(Some(scheme), value, _) => <key scheme={scheme.toString}>{value}</key>
      case QualifiedSchemedValue(None, value, _) => <key>{value}</key>
      case SchemedKeyValue(scheme, key, _) => <key scheme={scheme.toString}>{key}</key> // e.g. role, audience
      case PossiblySchemedKeyValue(Some(scheme), key, _) => <key scheme={scheme.toString}>{key}</key>
      case PossiblySchemedKeyValue(None, key, _) => <key>{key}</key>
      case a: AccessRights => <key>{a.category.toString}</key>
      case v => <key>{v}</key>
    }).copy(label = elemLabel(target, source)) // TODO attribute lang if available, for relations if it has a title, for authors to titles/organisation-name, otherwise to root
  }

  /**
   * @param target the default label
   * @param source may have a qualifier or some other property that determines the label
   */
  private def elemLabel[T](target: String, source: T) = {
    val rightsholder = "rightsholder"
    source match {
      case QualifiedSchemedValue(_, _, q: DateQualifier) if target == "otherDate" => q.toString
      case Author(_, _, _, _, Some(SchemedKeyValue(_, _, `rightsholder`)), _, _) => "dcterms:rightsholder"
      case _ => target
    }
  }

  private def authorDetails(a: Author, lang: Lang) =
    <dcx-dai:author>
      { optionalElem(a.titles, "dcx-dai:titles") }
      { optionalElem(a.initials, "dcx-dai:initials") }
      { optionalElem(a.insertions, "dcx-dai:insertions") }
      { optionalElem(a.surname, "dcx-dai:surname") }
      { optionalElem(a.role, "dcx-dai:role") }
      {
        a.organization.map(str =>
          <dcx-dai:organization>
            <dcx-dai:name>{str}</dcx-dai:name>
          </dcx-dai:organization>
        ).getOrElse(Seq.empty)
      }
    </dcx-dai:author>

  private def requiredElems[T](source: Option[Seq[T]], target: String, lang: Lang = None): Seq[Elem] = {
    source.map(_.map(elem(target, lang))).getOrElse(throwMandatory(target))
  }

  private def elems[T](source: Option[Seq[T]], target: String, lang: Lang = None): Seq[Elem] = {
    source.map(_.map(elem(target, lang))).getOrElse(Seq.empty)
  }

  private def optionalElem[T](source: Option[T], target: String, lang: Lang = None): Seq[Elem] = {
    source.map(elem(target, lang)).toSeq
  }

  private def filter[S, T](xs: Seq[QualifiedSchemedValue[S, T]],
                           q: Seq[T]
                          ): Seq[QualifiedSchemedValue[S, T]] = {
    xs.filter(x => q.contains(x.qualifier))
  }

  private def throwMandatory(tag: String) = {
    throw new IllegalArgumentException(s"no content for mandatory $tag")
  }
}
