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

import nl.knaw.dans.lib.string._

case class SchemedValue(scheme: Option[String],
                        value: Option[String],
                       ) extends OptionalValue {

  def hasValue: Boolean = value.exists(!_.isBlank)
}
object SchemedValue {
  def apply(scheme: String, value: String): SchemedValue = {
    SchemedValue(Some(scheme), Some(value))
  }
}

case class SchemedKeyValue(scheme: Option[String],
                           key: Option[String],
                           value: Option[String],
                          )extends OptionalValue{
  override def hasValue: Boolean = value.exists(!_.isBlank)
}

object SchemedKeyValue {
  def apply(scheme: String, key: String, value: String): SchemedKeyValue = {
    SchemedKeyValue(Some(scheme), Some(key), Some(value))
  }
}
