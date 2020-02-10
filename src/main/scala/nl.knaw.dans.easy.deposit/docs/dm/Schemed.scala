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

import nl.knaw.dans.easy.deposit.docs.CollectionUtils._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.easy.deposit.docs.dm.SchemedKeyValue.keyedSchemes
import nl.knaw.dans.lib.string._

case class SchemedValue(scheme: Option[String],
                        value: Option[String],
                       ) extends OptionalValue {

  /** @return this with Some-s of empty strings as None-s for proper pattern matches */
  def withCleanOptions: SchemedValue = this.copy(
    scheme = scheme.flatMap(_.toOption),
    value = value.flatMap(_.toOption),
  )
}
object SchemedValue {
  def apply(scheme: String, value: String): SchemedValue = {
    SchemedValue(Some(scheme), Some(value))
  }
}

case class SchemedKeyValue(scheme: Option[String],
                           key: Option[String],
                           value: Option[String],
                          ) extends OptionalValue {

  /** key if available together with scheme, value otherwise */
  lazy val keyOrValue: String = this.withCleanOptions match {
    case SchemedKeyValue(Some(_), Some(key), _) => key
    case SchemedKeyValue(None, Some(_), _) => throw new IllegalArgumentException(s"${getClass.getSimpleName} needs a scheme for a key ${toJson(this)}")
    case _ => value.orEmpty
  }

  lazy val keyOrNull: String = key.orNull

  override lazy val hasValue: Boolean = {
    if (schemeNeedsKey)
      key.exists(!_.isBlank)
    else value.exists(!_.isBlank)
  }

  lazy val schemeNeedsKey: Boolean = scheme.exists(keyedSchemes contains _)

  /** @return this with Some-s of empty strings as None-s for proper pattern matches */
  def withCleanOptions: SchemedKeyValue = this.copy(
    scheme = scheme.flatMap(_.toOption),
    key = key.flatMap(_.toOption),
    value = value.flatMap(_.toOption),
  )
}

object SchemedKeyValue {
  private val keyedSchemes = Seq("abr:ABRcomplex", "abr:ABRperiode", "dcterms:ISO3166")

  def apply(scheme: String, key: String, value: String): SchemedKeyValue = {
    SchemedKeyValue(Some(scheme), Some(key), Some(value))
  }
}
