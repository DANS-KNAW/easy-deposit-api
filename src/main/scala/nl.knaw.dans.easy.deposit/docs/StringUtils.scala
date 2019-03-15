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

import nl.knaw.dans.lib.string._

object StringUtils {

  implicit class OptionSeq[T](val sources: Option[Seq[T]]) extends AnyVal {
    def getNonEmpty: Seq[T] = sources.map(_.filter {
      case source: String => !source.isBlank
      case _ => true
    }).getOrElse(Seq.empty)
  }

  implicit class RichOption(val str: Option[String]) extends AnyVal {
    def getNonEmpty: Seq[String] = str.filterNot(_.isBlank).toSeq

    // null omits attribute rendering
    def collectOrNull: String = str.collect { case s if !s.isBlank => s.trim }.orNull

    def collectOrEmpty: String = str.collect { case s if !s.isBlank => s.trim }.getOrElse("")
  }
}
