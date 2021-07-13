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

import nl.knaw.dans.easy.deposit.Errors.InvalidDocumentException
import nl.knaw.dans.easy.deposit.docs.CollectionUtils._
import nl.knaw.dans.easy.deposit.docs.dm._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._

import scala.util.{ Failure, Try }
import scala.xml._

object DDM extends SchemedXml with DebugEnhancedLogging {
  override val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  override val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    val lang: String = dm.languageOfDescription.flatMap(_.key).orNull
    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
      xmlns:gml="http://www.opengis.net/gml"
      xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
      xmlns:ddm={schemaNameSpace}
      xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
      xsi:schemaLocation={s"$schemaNameSpace $schemaLocation"}
    >
      <ddm:profile>
        { dm.titles.flatMap(_.headOption).withValue.map(str => <dc:title xml:lang={ lang }>{ str }</dc:title>) }
        { dm.descriptions.withValue.map(str => <dcterms:description xml:lang={ lang }>{ str }</dcterms:description>) }
        { dm.instructionsForReuse.withValue.map(str => <ddm:description descriptionType="TechnicalInfo">{ str }</ddm:description>) }
        { dm.creators.withValue.map(author => <dcx-dai:creatorDetails>{ nestedXML(author, lang) }</dcx-dai:creatorDetails>) }
        { dm.datesCreated.withValue.map(src => <ddm:created>{ src.value.orEmpty }</ddm:created>) }
        { dm.datesAvailable.withValue.map(src => <ddm:available>{ src.value.orEmpty }</ddm:available>) }
        { dm.audiences.toSeq.flatten.map(_.key).withValue.map(key => <ddm:audience>{ key }</ddm:audience>) }
        { dm.accessRights.toSeq.map(src => <ddm:accessRights>{ src.toString }</ddm:accessRights>) }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.identifiers.withValue.map(id => <dcterms:identifier xsi:type={ id.scheme.orOmit }>{ id.value.orEmpty }</dcterms:identifier>) }
        { dm.titles.getOrElse(Seq.empty).drop(1).withValue.map(str => <dcterms:alternative xml:lang={ lang }>{ str }</dcterms:alternative>) }
        { dm.alternativeTitles.withValue.map(str => <dcterms:alternative xml:lang={ lang }>{ str }</dcterms:alternative>) }
        { dm.allRelations.withValue.map(basicContent(_, lang)) }
        { dm.contributors.withValue.map(author => <dcx-dai:contributorDetails>{ nestedXML(author, lang) }</dcx-dai:contributorDetails>) }
        { dm.authors.map(_.rightsHolder).withValue.map(str => <dcterms:rightsHolder>{ str }</dcterms:rightsHolder>) }
        { dm.publishers.withValue.map(str => <dcterms:publisher xml:lang={ lang }>{ str }</dcterms:publisher>) }
        { dm.sources.withValue.map(str => <dc:source xml:lang={ lang }>{ str }</dc:source>) }
        { dm.types.withValue.map(src => <dcterms:type xsi:type={ src.scheme.orOmit }>{ src.value.orEmpty }</dcterms:type>) }
        { dm.formats.withValue.map(src => <dcterms:format xsi:type={ src.scheme.orOmit }>{ src.value.orEmpty }</dcterms:format>) }
        { dm.subjects.withValue.map(basicContent(_, "subject", lang)) }
        { dm.temporalCoverages.withValue.map(basicContent(_, "temporal", lang)) }
        { dm.spatialCoverages.withValue.map(basicContent(_, "spatial", lang)) }
        { dm.otherDates.withValue.map(date => <label xsi:type={ date.scheme.orOmit }>{ date.value.orEmpty }</label>.withLabel(date.qualifierAsString)) }
        { dm.spatialPoints.withValue.map(point => <dcx-gml:spatial srsName={ point.srsName }>{ nestedXML(point) }</dcx-gml:spatial>) }
        { dm.spatialBoxes.withValue.map(point => <dcx-gml:spatial>{ nestedXML(point) }</dcx-gml:spatial>) }
        { dm.license.withValue.map(src => <dcterms:license xsi:type={ src.scheme.orOmit }>{ src.value.orEmpty }</dcterms:license>) }
        { dm.languagesOfFiles.toSeq.flatten.withFilter(_.keyOrValue.nonEmpty).map(src => <dcterms:language xsi:type ={ src.scheme.orOmit  }>{ src.keyOrValue }</dcterms:language>) }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }.recoverWith {
    case e: IllegalArgumentException => Failure(InvalidDocumentException("DatasetMetadata", e))
  }

  private def nestedXML(point: SpatialPoint): Elem = {
    <Point xmlns="http://www.opengis.net/gml">
        <pos>{ point.pos }</pos>
    </Point>
  }

  private def nestedXML(box: SpatialBox): Elem = {
    <boundedBy xmlns="http://www.opengis.net/gml">
        <Envelope srsName={ box.srsName }>
            <lowerCorner>{ box.lower }</lowerCorner>
            <upperCorner>{ box.upper }</upperCorner>
        </Envelope>
    </boundedBy>
  }

  private def basicContent(relation: RelationType, lang: String): Elem = {
    val cleanRelation = relation.withCleanOptions
    (cleanRelation match {
      case Relation(_, Some(url: String), None) =>
        <label href={ relation.urlOrNull }>{ url }</label>
      case rel: RelatedIdentifier if rel.url.isEmpty =>
        <label xsi:type={ rel.scheme.orOmit }>{ rel.value.orEmpty }</label>
      case rel: RelatedIdentifier =>
        <label scheme={ rel.scheme.orOmit } href={ rel.urlOrNull }>{ rel.value.orEmpty }</label>
      case rel: Relation => // Relation(_,None,None) is skipped so we do have a title (via value) and therefore a language
        <label xml:lang={ lang } href={ rel.urlOrNull }>{ rel.value.orEmpty }</label>
    }).withLabel(qualifier(cleanRelation))
  }

  private def qualifier(cleanRelation: RelationType) = {
    val qualifier = cleanRelation.qualifierAsString
    cleanRelation.url
      .map(_ => qualifier.replace("dcterms", "ddm"))
      .getOrElse(qualifier)
  }

  private def basicContent(source: SchemedKeyValue, label: String, lang: String): Elem = {
    (label, source.withCleanOptions) match {
      case ("subject", SchemedKeyValue(None, None, Some(value))) =>
        <label xml:lang={ lang }>{ value }</label>.withLabel(s"dc:$label")
      case (_, SchemedKeyValue(None, None, Some(value))) =>
        <label xml:lang={ lang }>{ value }</label>.withLabel(s"dcterms:$label")
      case (_, SchemedKeyValue(Some(scheme), Some(key), _)) if source.schemeNeedsKey =>
        <label xsi:type={ scheme }>{ key }</label>.withLabel(s"dcterms:$label")
      case (_, SchemedKeyValue(_, _, Some(value))) =>
        <label xml:lang={ lang } schemeURI={ source.scheme.orOmit } valueURI={ source.keyOrNull }>{ value }</label>
          .withLabel(s"ddm:$label")
    }
  }

  private def nestedXML(author: Author, lang: String): Seq[Node] = {
    if (author.surname.forall(_.isBlank))
      author.organization.toSeq.map(nestedXML(_, lang, author.role))
    else
      <dcx-dai:author>
        { author.titles.withValue.map(str => <dcx-dai:titles xml:lang={ lang }>{ str }</dcx-dai:titles>) }
        { author.initials.withValue.map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { author.insertions.withValue.map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        { author.surname.withValue.map(str => <dcx-dai:surname>{ str }</dcx-dai:surname>) }
        { author.ids.withValue.map(src => { <label>{ src.value.orEmpty }</label>.withLabel(daiLabel(src.scheme)) }) }
        { author.role.flatMap(_.key).withValue.map(key => <dcx-dai:role>{ key }</dcx-dai:role>) }
        { author.organization.withValue.map(nestedXML(_, lang, role = None)) }
      </dcx-dai:author>
  }

  private def daiLabel(empty: Option[String]) = {
    s"dcx-dai:${ empty.orEmpty.replace("id-type:", "") }"
  }

  private def nestedXML(organization: String, lang: String, role: Option[SchemedKeyValue]): Elem =
      <dcx-dai:organization>
        { <dcx-dai:name xml:lang={ lang }>{ organization }</dcx-dai:name> }
        { role.flatMap(_.key).withValue.map(key => <dcx-dai:role>{ key }</dcx-dai:role>) }
      </dcx-dai:organization>

  /** @param elem XML element to be adjusted */
  private implicit class RichElem(val elem: Elem) extends AnyVal {
    /** @param str the desired label, optionally with name space prefix */
    def withLabel(str: String): Elem = {
      str.split(":", 2) match {
        case Array(label) if label.nonEmpty => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
      }
    }
  }
}
