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

import nl.knaw.dans.easy.deposit.docs.dm.RelationQualifier.RelationQualifier
import nl.knaw.dans.lib.string._

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
  /** @return this with Some-s of empty strings as None-s */
  def withCleanOptions: RelationType

  def hasValue: Boolean
}

case class Relation(qualifier: Option[RelationQualifier],
                    url: Option[String],
                    title: Option[String],
                   ) extends RelationType with OptionalValue {
  override def withCleanOptions: RelationType = this.copy(
    url = url.flatMap(_.toOption),
    title = title.flatMap(_.toOption),
  )

  override def hasValue: Boolean = (
    url.flatMap(_.toOption),
    title.flatMap(_.toOption)
  ) match {
    case (None, None) => false
    case _ => true
  }
}

case class RelatedIdentifier(scheme: Option[String],
                             value: Option[String],
                             qualifier: Option[RelationQualifier],
                            ) extends RelationType with OptionalValue {
  override def withCleanOptions: RelationType = this.copy(
    scheme = scheme.flatMap(_.toOption),
    value = value.flatMap(_.toOption),
  )

  override def hasValue: Boolean = value.flatMap(_.toOption).fold(false)(_ => true)
}
