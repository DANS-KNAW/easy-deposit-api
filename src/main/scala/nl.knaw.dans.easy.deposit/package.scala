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
package nl.knaw.dans.easy

import java.nio.file.Paths

import better.files.StringExtensions
import nl.knaw.dans.bag.DansBag

import scala.util.{ Failure, Try }
import scala.xml._

package object deposit {

  val prologue = """<?xml version='1.0' encoding='UTF-8'?>"""

  implicit class XmlExtensions(val elem: Elem) extends AnyVal {

    def serialize: String = {
      prologue + "\n" + Utility.serialize(elem).toString
    }
  }
  implicit class BagExtensions(val bag: DansBag) extends AnyVal {
    def addMetadataFile(content: Elem, target: String): Try[Any] = {
      addMetadataFile(content.serialize, target)
    }

    def addMetadataFile(content: String, target: String): Try[Any] = {
      bag.addTagFile(content.inputStream, Paths.get(s"metadata/$target"))
    }
  }
  implicit class RichTries[T](val tries: TraversableOnce[Try[T]]) extends AnyVal {
    // TODO candidate for nl.knaw.dans.lib.error ?
    def failFastOr[R](onSuccess: => Try[R]): Try[R] = {
      tries
        .collectFirst { case Failure(e) => Failure(e) }
        .getOrElse(onSuccess)
    }
  }
}
