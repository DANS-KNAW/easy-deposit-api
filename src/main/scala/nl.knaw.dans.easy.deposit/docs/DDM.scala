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
import scala.xml.Utility.trim
import scala.xml._

object DDM extends SchemedXml with DebugEnhancedLogging {
  override val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  override val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"

  def apply(dm: DatasetMetadata): Try[Elem] = Try {
    val lang: String = dm.languageOfDescription.flatMap(_.key).nonBlankOrNull
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
        { dm.titles.withNonEmpty.map(str => <dc:title xml:lang={ lang }>{ str }</dc:title>) }
        { dm.descriptions.withNonEmpty.map(str => <dcterms:description xml:lang={ lang }>{ str }</dcterms:description>) }
        { dm.instructionsForReuse.withNonEmpty.map(str => <ddm:description descriptionType="TechnicalInfo">{ str }</ddm:description>) }
        { dm.creators.withNonEmpty.map(author => <dcx-dai:creatorDetails>{ details(author, lang) }</dcx-dai:creatorDetails>) }
        { dm.datesCreated.flatMap(_.value).withNonEmpty.map(str => <ddm:created>{ str }</ddm:created>) }
        { dm.datesAvailable.flatMap(_.value).withNonEmpty.map(str => <ddm:available>{ str }</ddm:available>) }
        { dm.audiences.toSeq.flatten.map(_.key).withNonEmpty.map(key => <ddm:audience>{ key }</ddm:audience>) }
        { dm.accessRights.toSeq.map(src => <ddm:accessRights>{ src.toString }</ddm:accessRights>) }
      </ddm:profile>
      <ddm:dcmiMetadata>
        { dm.identifiers.withNonEmpty.map(id => <dcterms:identifier xsi:type={ id.scheme.nonBlankOrNull }>{ id.value.nonBlankOrEmpty }</dcterms:identifier>) }
        { dm.alternativeTitles.withNonEmpty.map(str => <dcterms:alternative xml:lang={ lang }>{ str }</dcterms:alternative>) }
        { dm.allRelations.withNonEmpty.map(details(_, lang)) }
        { dm.contributors.withNonEmpty.map(author => <dcx-dai:contributorDetails>{ details(author, lang) }</dcx-dai:contributorDetails>) }
        { dm.rightsHolders.map(str => <dcterms:rightsHolder>{ str }</dcterms:rightsHolder>) }
        { dm.publishers.withNonEmpty.map(str => <dcterms:publisher xml:lang={ lang }>{ str }</dcterms:publisher>) }
        { dm.sources.withNonEmpty.map(str => <dc:source xml:lang={ lang }>{ str }</dc:source>) }
        { dm.types.withNonEmpty.map(src => <dcterms:type xsi:type={ src.scheme.nonBlankOrNull }>{ src.value.nonBlankOrEmpty }</dcterms:type>) }
        { dm.formats.withNonEmpty.map(src => <dcterms:format xsi:type={ src.scheme.nonBlankOrNull }>{ src.value.nonBlankOrEmpty }</dcterms:format>) }
        { dm.subjects.withNonEmpty.map(details(_, "subject", lang)) }
        { dm.temporalCoverages.withNonEmpty.map(details(_, "temporal", lang)) }
        { dm.spatialCoverages.withNonEmpty.map(details(_, "spatial", lang)) }
        { dm.otherDates.withNonEmpty.map(date => <label xsi:type={ date.scheme.nonBlankOrNull }>{ date.value.nonBlankOrEmpty }</label>.withLabel(date.qualifier)) }
        { dm.spatialPoints.withNonEmpty.map(point => <dcx-gml:spatial srsName={ point.srsName }>{ details(point) }</dcx-gml:spatial>) }
        { dm.spatialBoxes.withNonEmpty.map(point => <dcx-gml:spatial>{ details(point) }</dcx-gml:spatial>) }
        { dm.license.withNonEmpty.map(src => <dcterms:license xsi:type={ src.scheme.nonBlankOrNull }>{ src.value.nonBlankOrEmpty }</dcterms:license>) }
        { dm.languagesOfFiles.withNonEmpty.flatMap(languageDetails) }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }.recoverWith {
    case e: IllegalArgumentException => Failure(InvalidDocumentException("DatasetMetadata", e))
  }

  private def languageDetails(source: SchemedKeyValue): NodeSeq = {
    source match {
      case SchemedKeyValue(Some(_), Some(key), _) => <dcterms:language xsi:type ={ source.scheme.nonBlankOrNull  }>{ key }</dcterms:language>
      case SchemedKeyValue(_, _, Some(value)) => <dcterms:language xsi:type ={ source.scheme.nonBlankOrNull  }>{ value }</dcterms:language>
      case _ => NodeSeq.Empty
    }
  }

  private def details(point: SpatialPoint): Elem = {
    <Point xmlns="http://www.opengis.net/gml">
        <pos>{ point.pos }</pos>
    </Point>
  }

  private def details(box: SpatialBox): Elem = {
    <boundedBy xmlns="http://www.opengis.net/gml">
        <Envelope srsName={ box.srsName }>
            <lowerCorner>{ box.lower }</lowerCorner>
            <upperCorner>{ box.upper }</upperCorner>
        </Envelope>
    </boundedBy>
  }

  private def details(relation: RelationType, lang: String): Elem = {
    relation.withCleanOptions match {
      case Relation(_, Some(url: String), Some(title: String)) => <label xml:lang={ lang } href={ url }>{ title }</label>
      case Relation(_, Some(url: String), None) => <label href={ url }>{ url }</label>
      case Relation(_, None, Some(title: String)) => <label xml:lang={ lang }>{ title }</label>
      case RelatedIdentifier(Some("id-type:DOI"), Some(value), _) => <label scheme="id-type:DOI" href={ "https://doi.org/" + value }>{ value }</label>
      case RelatedIdentifier(Some("id-type:URN"), Some(value), _) => <label scheme="id-type:URN" href={ "http://persistent-identifier.nl/" + value }>{ value }</label>
      case RelatedIdentifier(Some(scheme @ ("id-type:URI" | "id-type:URL")), Some(value), _) => <label scheme={ scheme } href={ value }>{ value }</label>
      case RelatedIdentifier(scheme, Some(value), _) => <label xsi:type={ scheme.nonBlankOrNull }>{ value }</label>
      // should not get at the next because of withNonEmpty
      case _ => throw new IllegalArgumentException("invalid relation " + JsonUtil.toJson(relation))
    }
    }.withLabel(relation match { // replace the namespace in case of an href=URL attribute
    case RelatedIdentifier(_, _, None) => throw new IllegalArgumentException("missing qualifier: RelatedIdentifier" + JsonUtil.toJson(relation))
    case RelatedIdentifier(Some("id-type:URI" | "id-type:URL" | "id-type:URN" | "id-type:DOI"), _, Some(qualifier)) => qualifier.toString.replace("dcterms", "ddm")
    case RelatedIdentifier(_, _, Some(qualifier)) => qualifier.toString
    case Relation(None, _, _) => throw new IllegalArgumentException("missing qualifier: Relation" + JsonUtil.toJson(relation))
    case Relation(Some(qualifier), Some(_), _) => qualifier.toString.replace("dcterms", "ddm")
    case Relation(Some(qualifier), _, _) => qualifier.toString
    // should not get at the next because of withNonEmpty
    case _ => throw new IllegalArgumentException("invalid relation" + JsonUtil.toJson(relation))
  })

  private def details(source: SchemedKeyValue, label: String, lang: String): Elem = {
    val typeSchemes = Seq("abr:ABRcomplex", "abr:ABRperiode", "dcterms:ISO3166")
    (label, source) match {
      case ("subject", SchemedKeyValue(None, None, Some(value))) =>
        <label xml:lang={ lang }>{ value }</label>
          .withLabel(s"dc:$label")
      case (_, SchemedKeyValue(None, None, Some(value))) =>
        <label xml:lang={ lang }>{ value }</label>
          .withLabel(s"dcterms:$label")
      case (_, SchemedKeyValue(Some(scheme), Some(key), _)) if typeSchemes.contains(scheme) =>
        <label xsi:type={ scheme }>{ key }</label>
          .withLabel(s"dcterms:$label")
      case (_, SchemedKeyValue(_, _, Some(value))) =>
        <label xml:lang={ lang } schemeURI={ source.scheme.nonBlankOrNull } valueURI={ source.key.nonBlankOrNull }>{ value }</label>
          .withLabel(s"ddm:$label")
    }
  }

  private def details(author: Author, lang: String): Seq[Node] = {
    if (author.surname.forall(_.isBlank))
      author.organization.toSeq.map(orgDetails(_, lang, author.role))
    else
      <dcx-dai:author>
        { author.titles.withNonEmpty.map(str => <dcx-dai:titles xml:lang={ lang }>{ str }</dcx-dai:titles>) }
        { author.initials.withNonEmpty.map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { author.insertions.withNonEmpty.map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        { author.surname.withNonEmpty.map(str => <dcx-dai:surname>{ str }</dcx-dai:surname>) }
        { author.ids.withNonEmpty.map(src => <label>{ src.value.nonBlankOrNull }</label>.withLabel(s"dcx-dai:${ src.scheme.nonBlankOrEmpty.replace("id-type:", "") }")) }
        { author.role.flatMap(_.key).withNonEmpty.map(key => <dcx-dai:role>{ key }</dcx-dai:role>) }
        { author.organization.withNonEmpty.map(orgDetails(_, lang, role = None)) }
      </dcx-dai:author>
  }

  private def orgDetails(organization: String, lang: String, role: Option[SchemedKeyValue]): Elem =
      <dcx-dai:organization>
        { <dcx-dai:name xml:lang={ lang }>{ organization }</dcx-dai:name> }
        { role.flatMap(_.key).withNonEmpty.map(key => <dcx-dai:role>{ key }</dcx-dai:role>) }
      </dcx-dai:organization>

  /** @param elem XML element to be adjusted */
  private implicit class RichElem(val elem: Elem) extends AnyVal {
    /** @param str the desired label, optionally with name space prefix */
    @throws[InvalidDocumentException]("when str is not a valid XML label (has more than one ':')")
    def withLabel(str: String): Elem = {
      str.split(":") match {
        case Array(label) if label.nonEmpty => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
        case a => throw new IllegalArgumentException(
          s"expecting (label) or (prefix:label); got [${ a.mkString(":") }] to adjust the <${ elem.label }> of ${ trim(elem) }"
        )
      }
    }

    @throws[InvalidDocumentException]("when maybeVal does not contain a valid XML label (its .toString has more than one ':')")
    def withLabel[T](maybeVal: Option[T]): Elem = {
      withLabel(maybeVal.map(_.toString).nonBlankOrEmpty)
    }
  }
}
