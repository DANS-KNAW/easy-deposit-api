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

import scala.xml.Elem

trait PossiblySchemed {
  val scheme: Option[String]

  def schemeAsString: String = scheme match {
    case Some(s: String) if s.trim.nonEmpty => s.trim
    case _ => null // will suppress the XML attribute
  }
}

trait PossiblyKeyed {
  val key: Option[String]

  def keyAsString: String = key match {
    case Some(s: String) if s.trim.nonEmpty => s.trim
    case _ => null
  }
}

case class SchemedValue(override val scheme: Option[String],
                        value: Option[String],
                       ) extends PossiblySchemed
object SchemedValue {
  def apply(scheme: String, value: String): SchemedValue = {
    SchemedValue(Some(scheme), Some(value))
  }
}

case class SchemedKeyValue(override val scheme: Option[String],
                           override val key: Option[String],
                           value: Option[String],
                          ) extends PossiblySchemed with PossiblyKeyed

object SchemedKeyValue {
  def apply(scheme: String, key: String, value: String): SchemedKeyValue = {
    SchemedKeyValue(Some(scheme), Some(key), Some(value))
  }

  implicit class SchemedKeyValuesExtensions(val skv: Seq[SchemedKeyValue]) extends AnyVal {
    def collectKey(f: String => Elem): Seq[Elem] = skv.collect {
      case SchemedKeyValue(_, Some(key), _) if !key.isBlank => f(key)
    }
  }
}
