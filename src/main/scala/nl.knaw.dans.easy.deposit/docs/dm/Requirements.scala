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

import scala.util.Try

trait Requirements {
  def requireNonEmptyString[T](value: T): Unit = {
    value match {
      case str: String =>
        require(str.trim.nonEmpty, buildMsg("Empty string for a mandatory field"))
      case _ =>
    }
  }

  // using doubles as arguments could change precision in the XML output by adding ".0"
  protected def requireDouble(value: String): Unit = {
    require(Try(value.toDouble).isSuccess, buildMsg(s"Invalid number [$value]"))
  }

  protected def buildMsg(msg: String) = s"$msg; got ${ toJson(this) } ${ this.getClass.getSimpleName }"
}
