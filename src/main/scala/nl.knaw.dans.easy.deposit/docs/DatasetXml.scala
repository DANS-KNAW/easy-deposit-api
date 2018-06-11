/**
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.{ DateQualifier, available, created }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ DateQualifier, _ }
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scala.util.Try
import scala.xml.{ Attribute, Elem, Null, PrefixedAttribute }

object DatasetXml {
  private val otherDateQualifiers = DateQualifier.values.filter(qualifier =>
    qualifier != DateQualifier.created && qualifier != DateQualifier.available
  ).toSeq
  private val qualifier = "qualifier"

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    val lang = dm.languageOfDescription.map(l => new PrefixedAttribute("xml", "lang", l.key, Null))
    val dateSubmitted = QualifiedSchemedValue[String, DateQualifier](
      Some("dcterms:W3CDTF"),
      DateTime.now().toString(ISODateTimeFormat.date()),
      DateQualifier.dateSubmitted
    )
    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
    >
      <ddm:profile>
        { requiredElems(dm.titles, "dcterms:title").addAttr(lang) }
        { requiredElems(dm.descriptions, "dc:description").addAttr(lang) }
        { requiredElems(dm.creators, "dcx-dai:creatorDetails", lang) }
        { requiredElems(dm.dates.map(filter(_, Seq(created))), "ddm:created") }
        { requiredElems(dm.dates.map(filter(_, Seq(available))), "ddm:available") }
        { requiredElems(dm.audiences, "ddm:audience") }
        { requiredElems(dm.accessRights.map(Seq(_)), "ddm:accessRights") }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { elems(dm.alternativeTitles, "dcterms:alternative").addAttr(lang) }
        { elems(dm.contributors, "dcx-dai:creatorDetails", lang) }
        { elems(dm.publishers, "dcterms:publisher").addAttr(lang) }
        { elems(dm.sources, "dc:source").addAttr(lang) }
        { elems(dm.dates.map(filter(_, otherDateQualifiers)), qualifier) }
        { optionalElem(Some(dateSubmitted), qualifier) }
        { optionalElem(dm.license, "dcterms:license") /* TODO xsi:type="dcterms:URI" not supported by json */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }

  // called by requiredElems, elems, optionalElem
  private def elem[T](target: String, lang: Option[Attribute])(source: T): Elem = (source match {
    case a: Author => <key>{authorDetails(a, lang)}</key>
    case a: AccessRights => <key>{a.category.toString}</key>
    case SchemedKeyValue(_, key, _) => <key>{key}</key> // e.g. role, audience
    case QualifiedSchemedValue(None, value, _) => <key>{value}</key>
    case QualifiedSchemedValue(Some(scheme), value, _) if scheme == "dcterms:W3CDTF" => <key>{value}</key>
    case QualifiedSchemedValue(Some(scheme), value, _) => <key scheme={scheme.toString}>{value}</key>
    case PossiblySchemedKeyValue(Some(scheme), key, _) => <key scheme={scheme.toString}>{key}</key>
    case PossiblySchemedKeyValue(None, key, _) => <key>{key}</key>
    case v => <key>{v}</key>
  }).copy(label = elemLabel(target, source))

  /**
   * @param target the default label
   * @param source may have a qualifier or some other property that determines the label
   */
  private def elemLabel[T](target: String, source: T) = {
    val rightsholder = "rightsholder"
    source match {
      case x: QualifiedSchemedValue[_, _] if target == qualifier => x.qualifier.toString
      case Author(_, _, _, _, Some(SchemedKeyValue(_, _, `rightsholder`)), _, _) => "dcterms:rightsholder"
      case _ => target
    }
  }

  private def authorDetails(author: Author, lang: Option[Attribute]) =
    <dcx-dai:author>
      { optionalElem(author.titles, "dcx-dai:titles").addAttr(lang) }
      { optionalElem(author.initials, "dcx-dai:initials") }
      { optionalElem(author.insertions, "dcx-dai:insertions") }
      { optionalElem(author.surname, "dcx-dai:surname") }
      { optionalElem(author.role, "dcx-dai:role") }
      {
        author.organization.map(str =>
          <dcx-dai:organization>
            { <dcx-dai:name>{str}</dcx-dai:name>.addAttr(lang) }
          </dcx-dai:organization>
        ).getOrElse(Seq.empty)
      }
    </dcx-dai:author>

  private implicit class RichElem(elem: Elem) extends Object {
    def addAttr(maybeAttribute: Option[Attribute]): Elem = maybeAttribute.map(elem % _).getOrElse(elem)
  }

  private implicit class RichElems(elems: Seq[Elem]) extends Object {
    def addAttr(lang: Option[Attribute]): Seq[Elem] = elems.map(_.addAttr(lang))
  }

  private def requiredElems[T](source: Option[Seq[T]], target: String, lang: Option[Attribute] = None): Seq[Elem] = {
    source.map(_.map(elem(target, lang))).getOrElse(throwMandatory(target))
  }

  private def elems[T](source: Option[Seq[T]], target: String, lang: Option[Attribute] = None): Seq[Elem] = {
    source.map(_.map(elem(target, lang))).getOrElse(Seq.empty)
  }

  private def optionalElem[T](source: Option[T], target: String, lang: Option[Attribute] = None): Seq[Elem] = {
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