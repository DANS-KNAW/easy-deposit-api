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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.{ DateQualifier, available, created, dateSubmitted }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ DateQualifier, _ }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import org.joda.time.DateTime

import scala.util.Try
import scala.xml._

object DatasetXml {
  private val otherDateQualifiers = DateQualifier.values.filterNot { qualifier =>
    Seq(
      created, // in ddm:profile
      available, // in ddm:profile
      dateSubmitted // generated, ignore if in input
    ).contains(qualifier)
  }.toSeq

  // TODO see unit test for error handling why this is not private
  val targetFromQualifier = "qualifier"

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    val lang = dm.languageOfDescription.map(l => new PrefixedAttribute("xml", "lang", l.key, Null))
    val dateSubmitted = Date(DateTime.now(), DateQualifier.dateSubmitted)
    val rightsHoldingCreators = dm.creators.map(_.filter(isRightsHolder))
    val contributors = Some(dm.contributors.getOrElse(Seq.empty) ++ rightsHoldingCreators.getOrElse(Seq.empty))
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
        { requiredElems(dm.creators.map(_.filterNot(isRightsHolder)), "dcx-dai:creatorDetails", lang) }
        { requiredElems(dm.dates.map(filter(_, Seq(created))), "ddm:created") }
        { requiredElems(dm.dates.map(filter(_, Seq(available))), "ddm:available") }
        { requiredElems(dm.audiences, "ddm:audience") }
        { requiredElems(dm.accessRights.map(Seq(_)), "ddm:accessRights") }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { elems(dm.alternativeTitles, "dcterms:alternative").addAttr(lang) }
        { elems(contributors, "dcx-dai:contributorDetails", lang) }
        { elems(dm.publishers, "dcterms:publisher").addAttr(lang) }
        { elems(dm.sources, "dc:source").addAttr(lang) }
        { elems(dm.dates.map(filter(_, otherDateQualifiers)), targetFromQualifier) }
        { optionalElem(Some(dateSubmitted), targetFromQualifier) }
        { optionalElem(dm.license, "dcterms:license") /* TODO xsi:type="dcterms:URI" not supported by json */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }

  // called by requiredElems, elems, optionalElem
  private def elem[T](target: String, lang: Option[Attribute])(source: T): Elem = (source match {
    case a: Author if isRightsHolder(a) => <key>{a.toString}</key>
    case a: Author => <key>{authorDetails(a, lang)}</key>
    case a: AccessRights => <key>{a.category.toString}</key>
    case x: SchemedKeyValue[_] => <key>{x.key}</key> // e.g. role, audience
    case QualifiedSchemedValue(None, value, _) => <key>{value}</key>
    case QualifiedSchemedValue(_, value, _: DateQualifier) => <key>{value}</key>
    case QualifiedSchemedValue(Some(scheme), value, _) => <key scheme={scheme.toString}>{value}</key>
    case PossiblySchemedKeyValue(Some(scheme), key, _) => <key scheme={scheme.toString}>{key}</key>
    case PossiblySchemedKeyValue(None, key, _) => <key>{key}</key>
    case v => <key>{v}</key>
  }).setTag(target, source)

  private def authorDetails(author: Author, lang: Option[Attribute]) =
    <dcx-dai:author>
      { optionalElem(author.titles, "dcx-dai:titles").addAttr(lang) }
      { optionalElem(author.initials, "dcx-dai:initials") }
      { optionalElem(author.insertions, "dcx-dai:insertions") }
      { optionalElem(author.surname, "dcx-dai:surname") }
      { optionalElem(author.role, "dcx-dai:role") }
      { // TODO ID's
      author.organization.map(str =>
          <dcx-dai:organization>
            { <dcx-dai:name>{str}</dcx-dai:name>.addAttr(lang) }
          </dcx-dai:organization>
      ).getOrElse(Seq.empty)
      }
    </dcx-dai:author>

  /** @param elem XML element to be adjusted */
  implicit class RichElem(elem: Elem) extends Object {
    /**
     * @param target the default tag (namespace:label)
     * @param source may have a qualifier or some other property that determines the tag
     */
    def setTag[T](target: String, source: T): Elem = {
      (source match {
        case x: QualifiedSchemedValue[_, _] if target == targetFromQualifier => x.qualifier.toString
        case a: Author if isRightsHolder(a) => "dcterms:rightsHolder"
        case _ => target
      }).split(":") match {
        case Array(label) => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
        case a => throw InvalidDocumentException("DatasetMetadata", new Exception(
          s"expecting (label) or (prefix:label); got [${ a.mkString(":") }] to adjust the <key> of ${ Utility.trim(elem) } created from: $source"
        ))
      }
    }

    def addAttr(maybeAttribute: Option[Attribute]): Elem = maybeAttribute.map(elem % _).getOrElse(elem)
  }

  private def isRightsHolder[T](a: T with Author) = {
    a.role.exists(_.key == "RightsHolder")
  }
  /** @param elems the sequence of elements to adjust */
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
