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
package nl.knaw.dans.easy.deposit

import java.net.UnknownHostException

import better.files.StringExtensions
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{ Schema, SchemaFactory }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Try }
import scala.xml.{ Elem, PrettyPrinter, SAXParseException }

package object docs extends DebugEnhancedLogging {
  // pretty provides friendly trouble shooting for complex XML's
  val prettyPrinter: PrettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  implicit class triedSchemaExtension(val triedSchema: Try[Schema]) extends AnyVal {
    def isAvailable: Boolean = {
      triedSchema match {
        case Failure(e: SAXParseException) if e.getCause.isInstanceOf[UnknownHostException] => false
        case Failure(e: SAXParseException) if e.getMessage.contains("Cannot resolve") =>
          println("Probably an offline third party schema: " + e.getMessage)
          false
        case _ => true
      }
    }

    def validate(xml: Elem): Try[Unit] = {
      triedSchema.map(
        _.newValidator().validate(new StreamSource(
          prettyPrinter.format(xml).inputStream
        ))
      )
    }
  }

  implicit class SchemedXmlExtension(val schemdXml: SchemedXml) extends AnyVal {
    def loadSchema: Try[Schema] = Try {
      trace(schemdXml.schemaLocation)
      SchemaFactory
        .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
        .newSchema(Array(new StreamSource(schemdXml.schemaLocation)).toArray[Source])
    }
  }
}
