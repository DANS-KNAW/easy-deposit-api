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

  // TODO make private together with implicit class RichElem
  val targetFromQualifier = "qualifier"

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    implicit val lang: Option[Attribute] = dm.languageOfDescription.map(l => new PrefixedAttribute("xml", "lang", l.key, Null))
    val contributors = dm.contributors.optFlat.filterNot(_.isRightsHolder)
    val rightHolders = dm.contributors.optFlat.filter(_.isRightsHolder) ++ dm.creators.optFlat.filter(_.isRightsHolder)
    val otherDates = dm.dates.optFlat
      .filter(date => otherDateQualifiers.contains(date.qualifier)) :+
      Date(DateTime.now(), DateQualifier.dateSubmitted)

    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
    >
      <ddm:profile>
        { requiredElems(dm.titles, "dc:title").addAttr(lang) }
        { requiredElems(dm.descriptions, "dcterms:description").addAttr(lang) }
        { requiredElems(dm.creators.map(_.filterNot(_.isRightsHolder)), "dcx-dai:creatorDetails") }
        { requiredElems(dm.dates.map(filter(_, Seq(created))), "ddm:created") }
        { requiredElems(dm.dates.map(filter(_, Seq(available))), "ddm:available") }
        { requiredElems(dm.audiences, "ddm:audience") }
        { requiredElems(dm.accessRights.map(Seq(_)), "ddm:accessRights") }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.alternativeTitles.optFlat.map(str => <dcterms:alternative>{str}</dcterms:alternative>).addAttr(lang) }
        { contributors.map(author => <dcx-dai:contributorDetails>{authorDetails(author)}</dcx-dai:contributorDetails>) }
        { rightHolders.map(author => <dcterms:rightsHolder>{author.toString}</dcterms:rightsHolder>) }
        { dm.publishers.optFlat.map(str => <dcterms:publisher>{str}</dcterms:publisher>).addAttr(lang) }
        { dm.sources.optFlat.map(str => <dc:source>{str}</dc:source>).addAttr(lang) }
        { otherDates.filter(_.hasScheme).map(date => <x xsi:type={date.scheme.getOrElse("")}>{date.value}</x>.withLabel(date.qualifier.toString)) }
        { otherDates.filterNot(_.hasScheme).map(date => <x>{date.value}</x>.withLabel(date.qualifier.toString)) }
        { dm.license.toSeq.map(str => <dcterms:license>{str}</dcterms:license>) /* TODO xsi:type="dcterms:URI" not supported by json */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }

  // called by requiredElems, elems, optionalElem
  private def elem[T](target: String)(source: T)(implicit lang: Option[Attribute]): Elem = (source match {
    case a: Author if a.isRightsHolder => <key>{a.toString}</key>
    case a: Author => <key>{authorDetails(a)}</key>
    case a: AccessRights => <key>{a.category.toString}</key>
    case x: SchemedKeyValue[_] => <key>{x.key}</key> // e.g. role, audience
    case QualifiedSchemedValue(None, value, _) => <key>{value}</key>
    case QualifiedSchemedValue(Some(scheme), value, _: DateQualifier) if target == targetFromQualifier => <key xsi:type={scheme.toString}>{value}</key>
    case QualifiedSchemedValue(_, value, _: DateQualifier) => <key>{value}</key>
    case QualifiedSchemedValue(Some(scheme), value, _) => <key scheme={scheme.toString}>{value}</key>
    case PossiblySchemedKeyValue(Some(scheme), key, _) => <key scheme={scheme.toString}>{key}</key>
    case PossiblySchemedKeyValue(None, key, _) => <key>{key}</key>
    case v => <key>{v}</key>
  }).setTag(target, source)

  private def authorDetails(author: Author)
                           (implicit lang: Option[Attribute]) = {
    if (author.surname.isEmpty)
      author.organization.toSeq.map(orgDetails(author.role))
    else
      <dcx-dai:author>
        { optionalElem(author.titles, "dcx-dai:titles").addAttr(lang) }
        { requiredElems(author.initials.map(Seq(_)), "dcx-dai:initials") }
        { optionalElem(author.insertions, "dcx-dai:insertions") }
        { requiredElems(author.surname.map(Seq(_)), "dcx-dai:surname") }
        { optionalElem(author.role, "dcx-dai:role") }
        { author.organization.toSeq.map(orgDetails(role = None)) }
      </dcx-dai:author>
  }

  private def orgDetails(role: Option[SchemedKeyValue[String]])
                        (organization: String)
                        (implicit lang: Option[Attribute]) =
      <dcx-dai:organization>
        { role.toSeq.map(elem("dcx-dai:role")) }
        { requiredElems(Some(Seq(organization)), "dcx-dai:name").addAttr(lang) }
      </dcx-dai:organization>

  /** @param elem XML element to be adjusted */
  implicit class RichElem(elem: Elem) extends Object {
    // TODO make private once the thrown error can be tested through apply (when using targetFromQualifier for a non-enum)

    /**
     * @param target the default tag (namespace:label)
     * @param source may have a qualifier or some other property that determines the tag
     */
    @throws(classOf[InvalidDocumentException])
    def setTag[T](target: String, source: T): Elem = {
      (source match {
        case x: QualifiedSchemedValue[_, _] if target == targetFromQualifier => x.qualifier.toString
        case a: Author if a.isRightsHolder => "dcterms:rightsHolder"
        case _ => target
      }).split(":") match {
        case Array(label) => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
        case a => throw InvalidDocumentException("DatasetMetadata", new Exception(
          s"expecting (label) or (prefix:label); got [${ a.mkString(":") }] to adjust the <key> of ${ Utility.trim(elem) } created from: $source"
        ))
      }
    }

    def withLabel(str: String): Elem = elem.copy(label = str)
  }

  /** @param elems the sequence of elements to adjust */
  private implicit class RichElems(elems: Seq[Elem]) extends Object {
    def addAttr(lang: Option[Attribute]): Seq[Elem] = elems.map(elem => lang.map(elem % _).getOrElse(elem))

    def withLabel(str: String): Seq[Elem] = elems.map(_.copy(label = str))
  }

  implicit class RichOptElems[T](sources: Option[Seq[T]]) {
    def optFlat: Seq[T] = sources.toSeq.flatten.filterNot {
      case source: String => source.trim.isEmpty
      case _ => false
    }
  }

  private def requiredElems[T](source: Option[Seq[T]], target: String)
                              (implicit lang: Option[Attribute]): Seq[Elem] = {
    source.map(keepNonEmpty).filterNot(_.isEmpty).getOrElse {
      throw InvalidDocumentException("DatasetMetadata", new Exception(
        s"no content for mandatory $target"
      ))
    }.map(elem(target))
  }

  private def keepNonEmpty[T](ts: Seq[T]) = {
    ts.filter {
      case s: String => s.trim != ""
      case _ => true
    }
  }

  private def optionalElem[T](source: Option[T], target: String)
                             (implicit lang: Option[Attribute]): Seq[Elem] = {
    source.toSeq.map(elem(target))
  }

  private def filter[S, T](xs: Seq[QualifiedSchemedValue[S, T]],
                           q: Seq[T]
                          ): Seq[QualifiedSchemedValue[S, T]] = {
    xs.filter(x => q.contains(x.qualifier))
  }
}
