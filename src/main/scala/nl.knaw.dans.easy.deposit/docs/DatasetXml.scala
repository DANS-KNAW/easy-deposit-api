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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.DateQualifier.DateQualifier
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ DateQualifier, _ }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import org.joda.time.DateTime

import scala.util.Try
import scala.xml._

object DatasetXml {
  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    implicit val lang: Option[Attribute] = dm.languageOfDescription.map(l => new PrefixedAttribute("xml", "lang", l.key, Null))

    val authors = SubmittedAuthors(dm)
    val dates = SubmittedDates(dm)

    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
    >
      <ddm:profile>
        { dm.titles.getNonEmpty.map(src => <dc:title>{ src }</dc:title>).addAttr(lang).mustBeNonEmpty("dc:title") }
        { dm.descriptions.getNonEmpty.map(src => <dcterms:description>{ src }</dcterms:description>).addAttr(lang).mustBeNonEmpty("dc:title") }
        { authors.creators.map(author => <dcx-dai:creatorDetails>{ authorDetails(author) }</dcx-dai:creatorDetails>).mustBeNonEmpty("dcx-dai:creatorDetails") }
        { <ddm:created>{ dates.created.value }</ddm:created> }
        { <ddm:available>{ dates.available.value }</ddm:available> }
        { dm.audiences.getNonEmpty.map(src => <ddm:audience>{ src.key }</ddm:audience>).mustBeNonEmpty("ddm:audience") }
        { dm.accessRights.map(src => <ddm:accessRights>{ src.category.toString }</ddm:accessRights>).toSeq.mustBeNonEmpty("ddm:accessRights") }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.alternativeTitles.getNonEmpty.map(str => <dcterms:alternative>{ str }</dcterms:alternative>).addAttr(lang) }
        { authors.contributors.map(author => <dcx-dai:contributorDetails>{ authorDetails(author) }</dcx-dai:contributorDetails>) }
        { authors.rightsHolders.map(author => <dcterms:rightsHolder>{ author.toString }</dcterms:rightsHolder>) }
        { dm.publishers.getNonEmpty.map(str => <dcterms:publisher>{ str }</dcterms:publisher>).addAttr(lang) }
        { dm.sources.getNonEmpty.map(str => <dc:source>{ str }</dc:source>).addAttr(lang) }
        { dates.schemed.map(date => <x xsi:type={date.scheme.getOrElse("")}>{ date.value }</x>.withLabel(date.qualifier.toString)) }
        { dates.plain.map(date => <x>{ date.value }</x>.withLabel(date.qualifier.toString)) }
        { dm.license.getNonEmpty.map(str => <dcterms:license>{ str }</dcterms:license>) /* xsi:type="dcterms:URI" not supported by json */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }

  private def authorDetails(author: Author)
                           (implicit lang: Option[Attribute]) = {
    if (author.surname.isEmpty)
      author.organization.toSeq.map(orgDetails(author.role))
    else
      <dcx-dai:author>
        { author.titles.getNonEmpty.map(str => <dcx-dai:titles>{ str }</dcx-dai:titles>).addAttr(lang) }
        { author.initials.getNonEmpty.map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { author.insertions.getNonEmpty.map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        { author.surname.getNonEmpty.map(str => <dcx-dai:surname>{ str }</dcx-dai:surname>) }
        { author.role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { author.organization.getNonEmpty.map(orgDetails()) }
      </dcx-dai:author>
  }

  private def orgDetails(role: Option[SchemedKeyValue[String]] = None)
                        (organization: String)
                        (implicit lang: Option[Attribute]) =
      <dcx-dai:organization>
        { role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { <dcx-dai:name>{ organization }</dcx-dai:name>.addAttr(lang) }
      </dcx-dai:organization>

  private case class SubmittedAuthors(dm: DatasetMetadata) {
    val (rightsHoldingCreators, creators) = dm.creators.getOrElse(Seq.empty).partition(_.isRightsHolder)
    val (rightsHoldingContributors, contributors) = dm.contributors.getOrElse(Seq.empty).partition(_.isRightsHolder)
    val rightsHolders: Seq[Author] = rightsHoldingContributors ++ rightsHoldingCreators
  }

  private case class SubmittedDates(dm: DatasetMetadata) {
    private val flattenedDates: Seq[Date] = dm.dates.toSeq.flatten
    private val specialDateQualifiers = Seq(
      DateQualifier.created, // for ddm:profile
      DateQualifier.available, // for ddm:profile
    )
    val created: Date = getMandatorySingleDate(DateQualifier.created)
    val available: Date = getMandatorySingleDate(DateQualifier.available)
    val (schemed, plain) = {
      if (flattenedDates.exists(_.qualifier == DateQualifier.dateSubmitted))
        throw InvalidDocumentException(s"No ${ DateQualifier.dateSubmitted } allowed in DatasetMetadata")
      (flattenedDates :+ Date(DateTime.now(), DateQualifier.dateSubmitted)
        ).filterNot(date => specialDateQualifiers.contains(date.qualifier))
    }.partition(_.hasScheme)

    private def getMandatorySingleDate(qualifier: DateQualifier): Date = {
      val seq: Seq[Date] = flattenedDates.filter(_.qualifier == qualifier)
      if (seq.size > 1)
        throw InvalidDocumentException(s"Just one $qualifier allowed in DatasetMetadata")
      else seq.headOption
        .getOrElse(throw missingValue(qualifier.toString))
    }
  }

  /** @param elem XML element to be adjusted */
  implicit class RichElem(val elem: Elem) extends AnyVal {
    // TODO make private once the thrown error can be tested through apply (when using a non-enum for withLabel)

    /**
     * @param str the desired tag (namespace:label)
     */
    @throws[InvalidDocumentException]("when str is not a valid XML label (has more than one ':')")
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
  private implicit class RichElems(val elems: Seq[Elem]) extends AnyVal {
    def addAttr(lang: Option[Attribute]): Seq[Elem] = elems.map(_.addAttr(lang))

    def withLabel(str: String): Seq[Elem] = elems.map(_.copy(label = str))

    def mustBeNonEmpty(str: String): Seq[Elem] = {
      if (elems.isEmpty) throwNoContentFor(str)
      else elems
    }
  }

  private implicit class OptionSeq[T](val sources: Option[Seq[T]]) extends AnyVal {
    def getNonEmpty: Seq[T] = sources.map(_.filterNot {
      case source: String => source.trim.isEmpty
      case _ => false
    }).getOrElse(Seq.empty)
  }

  private implicit class RichOption[T](val sources: Option[T]) extends AnyVal {
    def getNonEmpty: Seq[T] = sources.toSeq.filterNot {
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
}
