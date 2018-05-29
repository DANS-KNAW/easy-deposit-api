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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ DateQualifier, _ }
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import org.json4s.Extraction.decompose
import org.json4s.JsonAST.{ JValue, _ }
import org.json4s.JsonDSL._
import org.json4s.ext.{ EnumNameSerializer, JodaTimeSerializers, UUIDSerializer }
import org.json4s.jackson.Serialization
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization.write
import org.json4s.{ CustomSerializer, DefaultFormats, Diff, Extraction, Formats, JsonInput }

import scala.reflect.runtime.universe.typeOf
import scala.util.{ Failure, Success, Try }

object JsonUtil {

  case class InvalidDocumentException(s: String, t: Throwable = null)
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

  class RelationTypeSerializer extends CustomSerializer[RelationType](_ =>
    ( {
      case JNull => null
      case s: JValue =>
        Try { Extraction.extract[Relation](s) }
          .getOrElse(Try { Extraction.extract[QualifiedSchemedValue](s) }
            .getOrElse(null)
          )
    }, {
      // case x: RelationType => JString(x.toString) // not enough for rejectNotExpectedContent
      case rel: Relation =>
        ("qualifier" -> rel.qualifier.toString) ~
          ("url" -> rel.url) ~
          ("title" -> rel.title)
      case rel: QualifiedSchemedValue =>
        ("scheme" -> rel.scheme) ~
          ("value" -> rel.value) ~
          ("qualifier" -> rel.qualifier)// TODO needs swagger an enum?
    }
    )
  )

  val enumerations = List(
    RelationQualifier,
    DateQualifier,
    State,
    AccessCategory,
    PrivacySensitiveDataPresent
  )

  private implicit val jsonFormats: Formats = new DefaultFormats {} +
    UUIDSerializer +
    new RelationTypeSerializer +
    new PathSerializer ++
    enumerations.map(new EnumNameSerializer(_)) ++
    JodaTimeSerializers.all

  implicit class RichJsonInput(body: JsonInput) {
    def deserialize[A: Manifest]: Try[A] = {
      for {
        parsed <- Try { JsonMethods.parse(body) }
        _ <- acceptOnlyJObject(parsed)
        result = extract[A](parsed)
        _ <- rejectNotExpectedContent(parsed, result)
      } yield result
    }.recoverWith { case t: Throwable =>
      val className = typeOf[A].typeSymbol.name.toString
      Failure(InvalidDocumentException(className, t))
    }


    private def rejectNotExpectedContent[T](parsed: JValue, extracted: T): Try[Unit] = {
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

  def extract[T: Manifest](parsed: JValue): T = Extraction.extract(parsed)

  def fromJsonString[T: Manifest](jsonString: String): Try[T] = Try(Serialization.read(jsonString))

  def fromJsonInput[T: Manifest](jsonInput: JsonInput): Try[T] = Try(Serialization.read(jsonInput))
}
