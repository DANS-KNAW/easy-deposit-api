/*
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

import nl.knaw.dans.easy.deposit.docs.dm.OptionalValue
import nl.knaw.dans.lib.string._

import scala.collection.generic.FilterMonadic

object CollectionUtils {

  implicit class RichSeq[T](val sources: Seq[T]) extends AnyVal {
    def withValue: FilterMonadic[T, Seq[T]] = sources.withFilter {
      case str: String => !str.isBlank
      case x: OptionalValue => x.hasValue
      case _ => true
    }
  }

  implicit class RichOptionSeq[T](val sources: Option[Seq[T]]) extends AnyVal {

    /** filters elements without a value (before mapping) */
    def withValue: FilterMonadic[T, Seq[T]] = sources.toSeq.flatten.withValue
  }

  implicit class RichSeqOption[T](val sources: Seq[Option[T]]) extends AnyVal {
    def withValue: FilterMonadic[T, Seq[T]] = sources.flatMap(_.toSeq).withValue
  }

  implicit class RichOption[T](val sources: Option[T]) extends AnyVal {
    def withValue: FilterMonadic[T, Seq[T]] = sources.toSeq.withValue

    def orEmpty: String = trimmed.getOrElse("")

    /** null omits attribute rendering */
    def orOmit: String = trimmed.orNull

    private def trimmed: Option[String] = sources.flatMap(_.toString.trim.toOption)
  }
}
