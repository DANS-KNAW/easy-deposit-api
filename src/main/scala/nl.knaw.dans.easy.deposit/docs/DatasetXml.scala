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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.{ available, created, dateSubmitted }
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

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    implicit val lang: Option[Attribute] = dm.languageOfDescription.map(l => new PrefixedAttribute("xml", "lang", l.key, Null))
    val accessRights = dm.accessRights.getOrElse(throwNoContentFor("ddm:accessRights"))
    val contributors = dm.contributors.optFlat.filterNot(_.isRightsHolder)
    val rightHolders = dm.contributors.optFlat.filter(_.isRightsHolder) ++ dm.creators.optFlat.filter(_.isRightsHolder)

    // TODO exactly one
    val dateCreated = dm.dates.optFlat.find(_.qualifier == created).getOrElse(throwNoContentFor("ddm:created"))
    val dateAvailable = dm.dates.optFlat.find(_.qualifier == available).getOrElse(throwNoContentFor("ddm:available"))

    val otherDates = dm.dates.optFlat
      .filter(date => otherDateQualifiers.contains(date.qualifier)) :+
      Date(DateTime.now(), DateQualifier.dateSubmitted)

    /**
     * TODO workaround: when inlining each element gets the global name space attributes
     * doesn't happen for dm.alternativeTitles.optFlat
     * @return simple XML element
     */
    def required[T <: String](sources: Option[Seq[T]], target: String): Seq[Elem] = {
      sources.reqFlat(target).map(src => <key>{ src }</key>.withLabel(target))
    }

    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
    >
      <ddm:profile>
        { required(dm.titles, "dc:title").addAttr(lang) }
        { required(dm.descriptions, "dcterms:description").addAttr(lang) }
        { dm.creators.reqFlat("dcx-dai:creatorDetails").map(author => <dcx-dai:creatorDetails>{ authorDetails(author) }</dcx-dai:creatorDetails>) }
        { <ddm:created>{ dateCreated.value }</ddm:created> }
        { <ddm:available>{ dateAvailable.value }</ddm:available> }
        { required(dm.audiences.map(_.map(_.key)), "ddm:audience") }
        { <ddm:accessRights>{ accessRights.category.toString }</ddm:accessRights> }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.alternativeTitles.optFlat.map(str => <dcterms:alternative>{ str }</dcterms:alternative>).addAttr(lang) }
        { contributors.map(author => <dcx-dai:contributorDetails>{ authorDetails(author) }</dcx-dai:contributorDetails>) }
        { rightHolders.map(author => <dcterms:rightsHolder>{ author.toString }</dcterms:rightsHolder>) }
        { dm.publishers.optFlat.map(str => <dcterms:publisher>{ str }</dcterms:publisher>).addAttr(lang) }
        { dm.sources.optFlat.map(str => <dc:source>{ str }</dc:source>).addAttr(lang) }
        { otherDates.filter(_.hasScheme).map(date => <x xsi:type={date.scheme.getOrElse("")}>{ date.value }</x>.withLabel(date.qualifier.toString)) }
        { otherDates.filterNot(_.hasScheme).map(date => <x>{ date.value }</x>.withLabel(date.qualifier.toString)) }
        { dm.license.toSeq.map(str => <dcterms:license>{ str }</dcterms:license>) /* xsi:type="dcterms:URI" not supported by json */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }

  private def authorDetails(author: Author)
                           (implicit lang: Option[Attribute]) = {
    if (author.surname.isEmpty)
      author.organization.toSeq.map(orgDetails(author.role))
    else
      <dcx-dai:author>
        { author.titles.toSeq.map(str => <dcx-dai:titles>{ str }</dcx-dai:titles>).addAttr(lang) }
        { author.initials.toSeq.map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { author.insertions.toSeq.map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        { author.surname.toSeq.map(str => <dcx-dai:surname>{ str }</dcx-dai:surname>) }
        { author.role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { author.organization.toSeq.map(orgDetails()) }
      </dcx-dai:author>
  }

  private def orgDetails(role: Option[SchemedKeyValue[String]] = None)
                        (organization: String)
                        (implicit lang: Option[Attribute]) =
      <dcx-dai:organization>
        { role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { <dcx-dai:name>{ organization }</dcx-dai:name>.addAttr(lang) }
      </dcx-dai:organization>

  /** @param elem XML element to be adjusted */
  implicit class RichElem(elem: Elem) extends Object {
    // TODO make private once the thrown error can be tested through apply (when using a non-enum for withLabel)

    /**
     * @param str the desired tag (namespace:label)
     */
    @throws(classOf[InvalidDocumentException])
    def withLabel(str: String): Elem = {
      str.split(":") match {
        case Array(label) => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
        case a => throw InvalidDocumentException("DatasetMetadata", new Exception(
          s"expecting (label) or (prefix:label); got [${ a.mkString(":") }] to adjust the <key> of ${ Utility.trim(elem) }"
        ))
      }
    }

    def addAttr(lang: Option[Attribute]): Elem = lang.map(elem % _).getOrElse(elem)
  }

  /** @param elems the sequence of XML elements to adjust */
  private implicit class RichElems(elems: Seq[Elem]) extends Object {
    def addAttr(lang: Option[Attribute]): Seq[Elem] = elems.map(_.addAttr(lang))

    def withLabel(str: String): Seq[Elem] = elems.map(_.copy(label = str))
  }

  implicit class OptionSeq[T](sources: Option[Seq[T]]) {
    def reqFlat(fieldName: String): Seq[T] = {
      if (sources.optFlat.nonEmpty)
        sources.getOrElse(Seq.empty)
      else throwNoContentFor(fieldName)
    }

    def optFlat: Seq[T] = sources.toSeq.flatten.filterNot {
      case source: String => source.trim.isEmpty
      case _ => false
    }
  }

  private def throwNoContentFor[T](fieldName: String) = {
    throw InvalidDocumentException(
      "DatasetMetadata",
      new Exception(s"no content for mandatory $fieldName")
    )
  }

  private def filter[S, T](xs: Seq[QualifiedSchemedValue[S, T]],
                           q: Seq[T]
                          ): Seq[QualifiedSchemedValue[S, T]] = {
    xs.filter(x => q.contains(x.qualifier))
  }
}
