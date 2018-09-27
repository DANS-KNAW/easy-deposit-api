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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.easy.deposit.docs.dm.DateQualifier.DateQualifier
import nl.knaw.dans.easy.deposit.docs.dm.{ Author, Date, DateQualifier }

import scala.util.Try
import scala.xml._

object DatasetXml {
  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    implicit val lang: Option[Attribute] = dm.languageOfDescription.map(l => new PrefixedAttribute("xml", "lang", l.key, Null))

    val authors = SubmittedAuthors(dm)
    val dates = SubmittedDates(dm)

    // validation like RichElems.mustBeNonEmpty and SubmittedDates.getMandatorySingleDate
    dm.doi.getOrElse(throwInvalidDocumentException(s"Please first GET a DOI for this deposit"))

    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
      xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
    >
      <ddm:profile>
        { dm.titles.getNonEmpty.map(src => <dc:title>{ src }</dc:title>).addAttr(lang).mustBeNonEmpty("a title") }
        { dm.descriptions.getNonEmpty.map(src => <dcterms:description>{ src }</dcterms:description>).addAttr(lang).mustBeNonEmpty("a description") }
        { authors.creators.map(author => <dcx-dai:creatorDetails>{ authorDetails(author) }</dcx-dai:creatorDetails>).mustBeNonEmpty("a creator") }
        { <ddm:created>{ dates.created.value }</ddm:created> }
        { <ddm:available>{ dates.available.value }</ddm:available> }
        { dm.audiences.getNonEmpty.map(src => <ddm:audience>{ src.key }</ddm:audience>).mustBeNonEmpty("an audience") }
        { dm.accessRights.map(src => <ddm:accessRights>{ src.category.toString }</ddm:accessRights>).toSeq.mustBeNonEmpty("the accessRights") }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.identifiers.getNonEmpty.map(id => <dcterms:identifier xsi:type={ id.scheme }>{ id.value }</dcterms:identifier>) }
        { dm.alternativeTitles.getNonEmpty.map(str => <dcterms:alternative>{ str }</dcterms:alternative>).addAttr(lang) }
        { authors.contributors.map(author => <dcx-dai:contributorDetails>{ authorDetails(author) }</dcx-dai:contributorDetails>) }
        { authors.rightsHolders.map(author => <dcterms:rightsHolder>{ author.toString }</dcterms:rightsHolder>) }
        { dm.publishers.getNonEmpty.map(str => <dcterms:publisher>{ str }</dcterms:publisher>).addAttr(lang) }
        { dm.sources.getNonEmpty.map(str => <dc:source>{ str }</dc:source>).addAttr(lang) }
        { dates.others.map(date => <x xsi:type={date.schemeAsString}>{ date.value }</x>.withLabel(date.qualifier.toString)) }
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
    if (flattenedDates.exists(_.qualifier == DateQualifier.dateSubmitted))
      throwInvalidDocumentException(s"No ${ DateQualifier.dateSubmitted } allowed")
    val created: Date = getMandatorySingleDate(DateQualifier.created)
    val available: Date = getMandatorySingleDate(DateQualifier.available)
    val others: Seq[Date] = Date.submitted() +:
      flattenedDates.filterNot(date =>
        specialDateQualifiers.contains(date.qualifier)
      )

    private def getMandatorySingleDate(qualifier: DateQualifier): Date = {
      val seq: Seq[Date] = flattenedDates.filter(_.qualifier == qualifier)
      if (seq.size > 1)
        throwInvalidDocumentException(s"Just one $qualifier allowed")
      else seq.headOption
        .getOrElse(throw missingValue(s"a date with qualifier: $qualifier"))
    }
  }

  /** @param elem XML element to be adjusted */
  implicit class RichElem(val elem: Elem) extends AnyVal {

    /** @param str the desired tag (namespace:label) or (label) */
    @throws[InvalidDocumentException]("when str is not a valid XML label (has more than one ':')")
    def withLabel(str: String): Elem = {
      str.split(":") match {
        case Array(label) => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
        case a => throwInvalidDocumentException(
          s"expecting (label) or (prefix:label); got [${ a.mkString(":") }] to adjust the <key> of ${ Utility.trim(elem) }"
        )
      }
    }

    def addAttr(lang: Option[Attribute]): Elem = lang.map(elem % _).getOrElse(elem)
  }

  private def throwInvalidDocumentException(msg: String) = {
    throw InvalidDocumentException("DatasetMetadata", new Exception(msg))
  }


  /** @param elems the sequence of XML elements to adjust */
  private implicit class RichElems(val elems: Seq[Elem]) extends AnyVal {
    def addAttr(lang: Option[Attribute]): Seq[Elem] = elems.map(_.addAttr(lang))

    def withLabel(str: String): Seq[Elem] = elems.map(_.copy(label = str))

    def mustBeNonEmpty(str: String): Seq[Elem] = {
      if (elems.isEmpty) throw missingValue(str)
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
}
