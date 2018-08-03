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

import java.lang.reflect.InvocationTargetException
import java.nio.file.{ Path, Paths }

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata._
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import org.json4s.Extraction.decompose
import org.json4s.JsonAST.{ JValue, _ }
import org.json4s.JsonDSL._
import org.json4s.ext.{ EnumNameSerializer, JodaTimeSerializers, UUIDSerializer }
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization.write
import org.json4s.{ CustomSerializer, DefaultFormats, Diff, Extraction, Formats, JsonInput }

import scala.reflect.runtime.universe.typeOf
import scala.util.{ Failure, Success, Try }

object JsonUtil {

  case class InvalidDocumentException(s: String, t: Throwable = null)
    extends Exception(if (t == null) s"invalid $s"
                      else s"invalid $s: ${ t.getClass } ${ t.getMessage }", t)

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
        // the first class should have a mandatory field that distinguishes it from the rest
        Try { Extraction.extract[RelatedIdentifier](s) }
          .orElse(Try { Extraction.extract[Relation](s) })
          .getOrElse(throw new IllegalArgumentException(s"expected one of (Relation | RelatedIdentifier) got: ${ toJson(s) }"))
    }, {
      // case x: RelationType => JString(x.toString) // would break rejectNotExpectedContent
      case Relation(qualifier, url, title) =>
        ("qualifier" -> qualifier.toString) ~
          ("url" -> url) ~
          ("title" -> title)
      case RelatedIdentifier(scheme, value, qualifier) =>
        ("scheme" -> scheme.map(_.toString)) ~
          ("value" -> value) ~
          ("qualifier" -> qualifier.toString)
    }
    )
  )

  val enumerations = List(
    RelationQualifier,
    DateQualifier,
    State,
    AccessCategory,
    PrivacySensitiveDataPresent,
  )

  private implicit val jsonFormats: Formats = new DefaultFormats {} +
    UUIDSerializer +
    new RelationTypeSerializer +
    new PathSerializer ++
    enumerations.map(new EnumNameSerializer(_)) ++
    JodaTimeSerializers.all

  // this cannot be inlined due to an experimental feature in Scala
  // similar problem + explanation: https://stackoverflow.com/a/20253865/2389405
  private def getType[A: Manifest] = typeOf[A].typeSymbol.name.toString

  implicit class RichJsonInput(val body: JsonInput) extends AnyVal {
    def deserialize[A: Manifest]: Try[A] = {
      for {
        parsed <- Try { JsonMethods.parse(body) }
        _ <- acceptOnlyJObject(parsed)
        result = Extraction.extract(parsed)
        _ <- rejectNotExpectedContent(parsed, result)
      } yield result
    }.recoverWith { case t: Throwable =>
      val cause = t.getCause match {
        // caused by require clauses of member classes
        case cause: InvocationTargetException if t.getMessage == "unknown error" => cause.getTargetException
        // when a a serializer of a superclass doesn't recognise any of the possibilities
        case cause: IllegalArgumentException if t.getMessage == "unknown error" => cause
        case _ => t
      }
      Failure(InvalidDocumentException(getType[A], cause))
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
}