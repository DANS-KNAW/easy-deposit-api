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

import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.lib.string._

trait OptionalValue {
  val value: Option[String]

  @throws[IllegalArgumentException]("when only some of the mandatory value parts are available")
  lazy val hasValue: Boolean = value.exists(!_.isBlank)

  @throws[IllegalArgumentException]("when the value was retrieved without a filter like 'withNonEmpty'.")
  lazy val valueOrThrow: String = value
    .collect { case s if !s.isBlank => s.trim }
    .getOrElse(throw new IllegalArgumentException(s"no value found for ${getClass.getSimpleName}: ${ toJson(this)}"))
}
