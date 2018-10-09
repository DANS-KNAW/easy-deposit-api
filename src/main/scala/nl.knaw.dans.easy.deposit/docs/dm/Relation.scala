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
package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ RequiresNonEmpty, _ }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.easy.deposit.docs.dm.RelationQualifier.RelationQualifier

object RelationQualifier extends Enumeration {
  type RelationQualifier = Value
  val hasFormat: RelationQualifier = Value("dcterms:hasFormat")
  val hasPart: RelationQualifier = Value("dcterms:hasPart")
  val hasVersion: RelationQualifier = Value("dcterms:hasVersion")
  val isFormatOf: RelationQualifier = Value("dcterms:isFormatOf")
  val isPartOf: RelationQualifier = Value("dcterms:isPartOf")
  val isReferencedBy: RelationQualifier = Value("dcterms:isReferencedBy")
  val isReplacedBy: RelationQualifier = Value("dcterms:isReplacedBy")
  val isRequiredBy: RelationQualifier = Value("dcterms:isRequiredBy")
  val isVersionOf: RelationQualifier = Value("dcterms:isVersionOf")
  val references: RelationQualifier = Value("dcterms:references")
  val relation: RelationQualifier = Value("dcterms:relation")
  val replaces: RelationQualifier = Value("dcterms:replaces")
  val requires: RelationQualifier = Value("dcterms:requires")
  val conformsTo: RelationQualifier = Value("dcterms:conformsTo")
}

trait RelationType {
  val qualifier: RelationQualifier

  /** @return this with Some-s of empty strings as None-s */
  def withCleanOptions: RelationType
}

case class Relation(override val qualifier: RelationQualifier,
                    url: Option[String],
                    title: Option[String],
                   ) extends RelationType with RequiresNonEmpty {
  require(title.isProvided || url.isProvided, s"Relation needs at least one of (title | url) got: ${ toJson(this) }")
  requireNonEmptyString(qualifier, "qualifier")

  override def withCleanOptions: RelationType = this.copy(
    url = url.find(_.trim.nonEmpty),
    title = title.find(_.trim.nonEmpty),
  )
}

case class RelatedIdentifier(override val scheme: Option[String],
                             value: String,
                             override val qualifier: RelationQualifier
                            ) extends RelationType with PossiblySchemed with RequiresNonEmpty {
  requireNonEmptyString(value, "value")
  requireNonEmptyString(qualifier, "qualifier")

  override def withCleanOptions: RelationType = this
}
