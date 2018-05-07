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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ AccessCategory, PrivacySensitiveDataPresent }
import nl.knaw.dans.easy.deposit.{ State, StateInfo }
import org.json4s
import org.json4s.Extraction.decompose
import org.json4s.JsonAST._
import org.json4s.ext.{ EnumNameSerializer, JodaTimeSerializers, UUIDSerializer }
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization.write
import org.json4s.{ CustomSerializer, DefaultFormats, Diff, Formats, JsonInput }

import scala.util.{ Failure, Success, Try }

object Json {

  case class InvalidDocumentException(s: String, t: Throwable)
    extends Exception(s"invalid $s: ${ t.getClass } ${ t.getMessage }", t)

  class PathSerializer extends CustomSerializer[Path](_ =>
    ( {
      case JString(s) => Paths.get(s)
      case JNull => null
    }, {
      case x: Path => JString(x.toString)
    }
    )
  )

  private implicit val jsonFormats: Formats = new DefaultFormats {} +
    UUIDSerializer +
    new PathSerializer +
    new EnumNameSerializer(State) +
    new EnumNameSerializer(AccessCategory) +
    new EnumNameSerializer(PrivacySensitiveDataPresent) ++
    JodaTimeSerializers.all

  private implicit class RichJsonInput(body: JsonInput) {
    def deserialize[A](docType: String)(implicit mf: scala.reflect.Manifest[A]): Try[A] = {
      for {
        parsed <- Try { JsonMethods.parse(body) }
        _ <- acceptOnlyJObject(parsed)
        result = parsed.extract[A]
        _ <- rejectNotExpectedContent(parsed, result)
      } yield result
    }.recoverWith {
      case t: Throwable => Failure(InvalidDocumentException(docType, t))
    }

    private def rejectNotExpectedContent[T](parsed: json4s.JValue, extracted: T): Try[Unit] = {
      decompose(extracted) diff parsed match {
        case Diff(_, JNothing, _) => Success(())
        case Diff(_, ignored, _) => Failure(new Exception(s"don't recognize ${ write(ignored) }"))
      }
    }

    private def acceptOnlyJObject(parsed: JValue): Try[Unit] = {
      if (parsed.isInstanceOf[JObject]) Success(())
      else Failure(new Exception(s"expected an object, got a ${ parsed.getClass }"))
    }
  }

  def toJson[A <: AnyRef](a: A): String = {
    // seems not to need a try: while the date formatter wasn't in place it produced empty strings
    write(a)
  }

  def getUser(body: JsonInput): Try[UserInfo] = body.deserialize[UserInfo]("UserInfo")

  def getStateInfo(body: JsonInput): Try[StateInfo] = body.deserialize[StateInfo]("StateInfo")

  def getDatasetMetadata(body: JsonInput): Try[DatasetMetadata] = body.deserialize[DatasetMetadata]("DatasetMetadata")

  def getDepositInfo(body: JsonInput): Try[DepositInfo] = body.deserialize[DepositInfo]("DepositInfo")
}
