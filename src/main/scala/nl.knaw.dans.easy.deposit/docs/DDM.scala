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
import nl.knaw.dans.easy.deposit.docs.dm._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.xml.sax.SAXParseException
import resource.{ ManagedResource, Using }
import nl.knaw.dans.lib.string._

import scala.util.{ Failure, Try }
import scala.xml._

object DDM extends DebugEnhancedLogging {
  val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/2018/05/ddm.xsd" // TODO property?

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    dm.doi.getOrElse(throw invalidDatasetMetadataException(new Exception(s"Please first GET a DOI for this deposit")))

    val lang: String = dm.languageOfDescription.map(_.key).orNull // null omits attribute rendering
    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
      xmlns:gml="http://www.opengis.net/gml"
      xmlns:ddm={schemaNameSpace}
      xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
      xsi:schemaLocation={s"$schemaNameSpace $schemaLocation"}
    >
      <ddm:profile>
        { dm.titles.getNonEmpty.map(src => <dc:title xml:lang={ lang }>{ src }</dc:title>) }
        { dm.descriptions.getNonEmpty.map(src => <dcterms:description xml:lang={ lang }>{ src }</dcterms:description>) }
        { dm.creatorsWithoutRights.map(author => <dcx-dai:creatorDetails>{ details(author, lang) }</dcx-dai:creatorDetails>) }
        { dm.datesCreated.map(src => <ddm:created>{ src.value }</ddm:created>) }
        { dm.datesAvailable.map(src => <ddm:available>{ src.value }</ddm:available>) }
        { dm.audiences.getNonEmpty.map(src => <ddm:audience>{ src.key }</ddm:audience>) }
        { dm.accessRights.toSeq.map(src => <ddm:accessRights>{ src.category.toString }</ddm:accessRights>) }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.allIdentifiers.map(id => <dcterms:identifier xsi:type={ id.scheme }>{ id.value }</dcterms:identifier>) }
        { dm.alternativeTitles.getNonEmpty.map(str => <dcterms:alternative xml:lang={ lang }>{ str }</dcterms:alternative>) }
        { dm.relations.getNonEmpty.map(src => details(src, lang)) }
        { dm.contributorsWithoutRights.map(author => <dcx-dai:contributorDetails>{ details(author, lang) }</dcx-dai:contributorDetails>) }
        { dm.rightsHolders.map(author => <dcterms:rightsHolder>{ author.toString }</dcterms:rightsHolder>) }
        { dm.publishers.getNonEmpty.map(str => <dcterms:publisher xml:lang={ lang }>{ str }</dcterms:publisher>) }
        { dm.sources.getNonEmpty.map(str => <dc:source xml:lang={ lang }>{ str }</dc:source>) }
        { dm.allTypes.map(src => <dcterms:type xsi:type={ src.schemeAsString }>{ src.value }</dcterms:type>) }
        { dm.formats.getNonEmpty.map(src => <dcterms:format xsi:type={ src.schemeAsString }>{ src.value }</dcterms:format>) }
        { dm.otherDates.map(date => <tag xsi:type={ date.schemeAsString }>{ date.value }</tag>.withTag(date.qualifier.toString)) }
        { dm.spatialPoints.getNonEmpty.map(point => <dcx-gml:spatial srsName={ point.srsName }>{ details(point) }</dcx-gml:spatial>) }
        { dm.spatialBoxes.getNonEmpty.map(point => <dcx-gml:spatial>{ details(point) }</dcx-gml:spatial>) }
        { dm.license.getNonEmpty.map(str => <dcterms:license>{ str }</dcterms:license>) /* xsi:type="dcterms:URI" not supported by json */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }.flatMap(validate)

  private def details(point: SpatialPoint) = {
    <Point xmlns="http://www.opengis.net/gml">
        <pos>{ point.pos }</pos>
    </Point>
  }

  private def details(box: SpatialBox) = {
    <boundedBy xmlns="http://www.opengis.net/gml">
        <Envelope srsName={ box.srsName }>
            <lowerCorner>{ box.lower }</lowerCorner>
            <upperCorner>{ box.upper }</upperCorner>
        </Envelope>
    </boundedBy>
  }

  private def details(relation: RelationType, lang: String): Elem = {
    relation.withCleanOptions match {
      case Relation(_, Some(url: String), Some(title: String)) => <tag xml:lang={ lang } href={ url }>{ title }</tag>
      case Relation(_, Some(url: String), None) => <tag href={ url }>{ url }</tag>
      case Relation(_, None, Some(title: String)) => <tag xml:lang={ lang }>{ title }</tag>
      case relatedID: RelatedIdentifier => <tag  xsi:type={ relatedID.schemeAsString }>{ relatedID.value }</tag>
    }
  }.withTag(relation match { // replace the name space in case of an URL attribute
    case Relation(qualifier, Some(_), _) => qualifier.toString.replace("dcterms", "ddm")
    case _ => relation.qualifier.toString
  })

  private def details(author: Author, lang: String): Seq[Node] = {
    if (author.surname.isEmpty)
      author.organization.toSeq.map(orgDetails(_, lang, author.role))
    else // TODO ids
      <dcx-dai:author>
        { author.titles.getNonEmpty.map(str => <dcx-dai:titles xml:lang={ lang }>{ str }</dcx-dai:titles>) }
        { author.initials.getNonEmpty.map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { author.insertions.getNonEmpty.map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        { author.surname.getNonEmpty.map(str => <dcx-dai:surname>{ str }</dcx-dai:surname>) }
        { author.role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { author.organization.getNonEmpty.map(orgDetails(_, lang, role = None)) }
      </dcx-dai:author>
  }

  private def orgDetails(organization: String, lang: String, role: Option[SchemedKeyValue]): Elem =
      <dcx-dai:organization>
        { role.toSeq.map(role => <dcx-dai:role>{ role.key }</dcx-dai:role>) }
        { <dcx-dai:name xml:lang={ lang }>{ organization }</dcx-dai:name> }
      </dcx-dai:organization>

  /** @param elem XML element to be adjusted */
  implicit class RichElem(val elem: Elem) extends AnyVal {
    // not private for testing purposes

    /** @param str the desired tag (namespace:label) or (label) */
    @throws[InvalidDocumentException]("when str is not a valid XML label (has more than one ':')")
    def withTag(str: String): Elem = {
      str.split(":") match {
        case Array(label) => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
        case a => throw invalidDatasetMetadataException(new Exception(
          s"expecting (label) or (prefix:label); got [${ a.mkString(":") }] to adjust the <key> of ${ Utility.trim(elem) }"
        ))
      }
    }
  }

  private implicit class OptionSeq[T](val sources: Option[Seq[T]]) extends AnyVal {
    def getNonEmpty: Seq[T] = sources.map(_.filterNot {
      case source: String => source.toOption.isEmpty
      case _ => false
    }).getOrElse(Seq.empty)
  }

  private implicit class RichOption[T](val sources: Option[T]) extends AnyVal {
    def getNonEmpty: Seq[T] = sources.toSeq.filterNot {
      case source: String => source.toOption.isEmpty
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
    logger.debug(prettyPrinter.format(ddm))
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
    }

  private def managedInputStream(ddm: Elem): ManagedResource[BufferedInputStream] = {
    Using.bufferedInputStream(
      new ByteArrayInputStream(
        prettyPrinter
          .format(ddm)
          .getBytes(StandardCharsets.UTF_8)
      ))
  }

  private def invalidDatasetMetadataException(exception: Exception) = {
    InvalidDocumentException("DatasetMetadata", exception)
  }

  case class SchemaNotAvailableException(t: Throwable)
    extends Exception(s"Schema's for validation not available, please try again later.", t)
}
