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

import java.io.{ BufferedInputStream, ByteArrayInputStream }
import java.net.UnknownHostException
import java.nio.charset.StandardCharsets

import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{ Schema, SchemaFactory }
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.easy.deposit.docs.dm.DateQualifier.DateQualifier
import nl.knaw.dans.easy.deposit.docs.dm.{ Author, DateQualifier }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.xml.sax.SAXParseException
import resource.{ ManagedResource, Using }

import scala.util.{ Failure, Try }
import scala.xml._

object DDM extends DebugEnhancedLogging {
  val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    implicit val lang: Option[Attribute] = dm.languageOfDescription.map(l => new PrefixedAttribute("xml", "lang", l.key, Null))

    // validation like mustBeNonEmpty and mustHaveOne
    dm.doi.getOrElse(throwInvalidDocumentException(s"Please first GET a DOI for this deposit"))

    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
      xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
      xsi:schemaLocation={s"$schemaNameSpace $schemaLocation"}
    >
      <ddm:profile>
        { dm.titles.getNonEmpty.map(src => <dc:title>{ src }</dc:title>).addAttr(lang).mustBeNonEmpty("a title") }
        { dm.descriptions.getNonEmpty.map(src => <dcterms:description>{ src }</dcterms:description>).addAttr(lang).mustBeNonEmpty("a description") }
        { dm.creatorsWithoutRights.map(author => <dcx-dai:creatorDetails>{ authorDetails(author) }</dcx-dai:creatorDetails>).mustBeNonEmpty("a creator") }
        { dm.datesCreated.map(src => <ddm:created>{ src.value }</ddm:created>).mustHaveOne(DateQualifier.dateSubmitted) }
        { dm.datesAvailable.map(src => <ddm:available>{ src.value }</ddm:available>).mustHaveOne(DateQualifier.available) }
        { dm.audiences.getNonEmpty.map(src => <ddm:audience>{ src.key }</ddm:audience>).mustBeNonEmpty("an audience") }
        { dm.accessRights.map(src => <ddm:accessRights>{ src.category.toString }</ddm:accessRights>).toSeq.mustBeNonEmpty("the accessRights") }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.allIdentifiers.map(id => <dcterms:identifier xsi:type={ id.scheme }>{ id.value }</dcterms:identifier>) }
        { dm.alternativeTitles.getNonEmpty.map(str => <dcterms:alternative>{ str }</dcterms:alternative>).addAttr(lang) }
        { dm.contributorsWithoutRights.map(author => <dcx-dai:contributorDetails>{ authorDetails(author) }</dcx-dai:contributorDetails>) }
        { dm.rightsHolders.map(author => <dcterms:rightsHolder>{ author.toString }</dcterms:rightsHolder>) }
        { dm.publishers.getNonEmpty.map(str => <dcterms:publisher>{ str }</dcterms:publisher>).addAttr(lang) }
        { dm.sources.getNonEmpty.map(str => <dc:source>{ str }</dc:source>).addAttr(lang) }
        { dm.allTypes.map(src => <dcterms:type xsi:type={ src.schemeAsString }>{ src.value }</dcterms:type>) }
        { dm.formats.getNonEmpty.map(src => <dcterms:format xsi:type={ src.schemeAsString }>{ src.value }</dcterms:format>) }
        { dm.otherDates.map(date => <x xsi:type={ date.schemeAsString }>{ date.value }</x>.withLabel(date.qualifier.toString)) }
        { dm.license.getNonEmpty.map(str => <dcterms:license>{ str }</dcterms:license>) /* xsi:type="dcterms:URI" not supported by json */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }.flatMap(validate)

  private def authorDetails(author: Author)
                           (implicit lang: Option[Attribute]) = {
    if (author.surname.isEmpty)
      author.organization.toSeq.map(orgDetails(author.role))
    else // TODO ids
      <dcx-dai:author>
        { author.titles.getNonEmpty.map(str => <dcx-dai:titles>{ str }</dcx-dai:titles>).addAttr(lang) }
        { author.initials.getNonEmpty.map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { author.insertions.getNonEmpty.map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        { author.surname.getNonEmpty.map(str => <dcx-dai:surname>{ str }</dcx-dai:surname>) }
        { author.role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { author.organization.getNonEmpty.map(orgDetails()) }
      </dcx-dai:author>
  }

  private def orgDetails(role: Option[SchemedKeyValue] = None)
                        (organization: String)
                        (implicit lang: Option[Attribute]) =
      <dcx-dai:organization>
        { role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { <dcx-dai:name>{ organization }</dcx-dai:name>.addAttr(lang) }
      </dcx-dai:organization>

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

  /** @param elems the sequence of XML elements to adjust */
  private implicit class RichElems(val elems: Seq[Elem]) extends AnyVal {
    def addAttr(lang: Option[Attribute]): Seq[Elem] = elems.map(_.addAttr(lang))

    def withLabel(str: String): Seq[Elem] = elems.map(_.copy(label = str))

    def mustBeNonEmpty(str: String): Seq[Elem] = {
      if (elems.isEmpty) throw missingValue(str)
      else elems
    }

    def mustHaveOne(dateQualifier: DateQualifier): Seq[Elem] = elems match {
      case Seq() => throw missingValue(dateQualifier.toString)
      case Seq(_) => elems
      case _ => throwInvalidDocumentException(s"Just one $dateQualifier allowed")
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

  // pretty provides friendly trouble shooting for complex XML's
  private val prettyPrinter: PrettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  lazy val triedSchema: Try[Schema] = Try { // loading postponed until we actually start validating
    SchemaFactory
      .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      .newSchema(Array(new StreamSource(schemaLocation)).toArray[Source])
  }

  private def validate(ddm: Elem): Try[Elem] = {
    triedSchema.map(schema =>
      managedInputStream(ddm)
        .apply(inputStream => schema
          .newValidator()
          .validate(new StreamSource(inputStream))
        )
    )
  }.map(_ => ddm)
    .recoverWith {
      case e: SAXParseException if e.getCause.isInstanceOf[UnknownHostException] =>
        logger.error(e.getMessage, e)
        Failure(SchemaNotAvailableException(e))
      case e: SAXParseException => Failure(invalidDatasetMetadataException(e))
      case e => Failure(e)
    }

  def managedInputStream(ddm: Elem): ManagedResource[BufferedInputStream] = {
    Using.bufferedInputStream(
      new ByteArrayInputStream(
        prettyPrinter
          .format(ddm)
          .getBytes(StandardCharsets.UTF_8)
      ))
  }

  private def throwInvalidDocumentException(msg: String) = {
    throw invalidDatasetMetadataException(new Exception(msg))
  }

  private def invalidDatasetMetadataException(exception: Exception) = {
    InvalidDocumentException("DatasetMetadata", exception)
  }

  case class SchemaNotAvailableException(t: Throwable = null)
    extends Exception(s"Schema's for validation not available, please try again later.", t)
}
