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

import java.nio.file.{ Path, Paths }
import java.text.SimpleDateFormat

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ AccessCategory, PrivacySensitiveDataPresent }
import nl.knaw.dans.easy.deposit.{ DepositInfo, State, StateInfo }
import org.json4s
import org.json4s.JsonAST._
import org.json4s.ext.{ EnumNameSerializer, JodaTimeSerializers, UUIDSerializer }
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization.write
import org.json4s.{ CustomSerializer, DefaultFormats, Formats, JsonInput }

import scala.util.{ Failure, Try }

object Json {

  case class InvalidDocumentException(s: String, t: Throwable)
    extends Exception(s"invalid $s: ${t.getClass} ${t.getMessage}", t)

  class PathSerializer extends CustomSerializer[Path](_ =>
    ( {
      case JString(s) => Paths.get(s)
      case JNull => null
    }, {
      case x: Path => JString(x.toString)
    }
    )
  )

  private implicit val jsonFormats: Formats = new DefaultFormats {
    // we need a timestamp for DepositInfo, dates in DatasetMetadata are plain strings so no conflict
    override protected def dateFormatter: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
  } +
    UUIDSerializer +
    new PathSerializer +
    new EnumNameSerializer(State) +
    new EnumNameSerializer(AccessCategory) +
    new EnumNameSerializer(PrivacySensitiveDataPresent) ++
    JodaTimeSerializers.all

  def toJson[A <: AnyRef](a: A): String = {
    // seems not to need a try: while the date formatter wasn't in place it produced empty strings
    write(a)
  }

  def getUser(body: JsonInput): Try[UserInfo] = {
    parseObject(body).map(_.extract[UserInfo])
  }.recoverWith { case t: Throwable => Failure(InvalidDocumentException("User", t)) }

  def getStateInfo(body: JsonInput): Try[StateInfo] = {
    parseObject(body).map(_.extract[StateInfo])
  }.recoverWith { case t: Throwable => Failure(InvalidDocumentException("StateInfo", t)) }

  def getDatasetMetadata(body: JsonInput): Try[DatasetMetadata] = {
    parseObject(body).map(_.extract[DatasetMetadata])
  }.recoverWith { case t: Throwable => Failure(InvalidDocumentException("DatasetMetadata", t)) }

  def getDepositInfo(body: JsonInput): Try[DepositInfo] = {
    parseObject(body).map(_.extract[DepositInfo])
  }.recoverWith { case t: Throwable => Failure(InvalidDocumentException("DepositInfo", t)) }

  private def parseObject(body: JsonInput): Try[json4s.JValue] = Try {
    JsonMethods.parse(body)
  }.map {
    case jObject if jObject.isInstanceOf[JObject] => jObject
    case jValue => throw new Exception(s"expected an object, got a ${ jValue.getClass }")
  }
}
