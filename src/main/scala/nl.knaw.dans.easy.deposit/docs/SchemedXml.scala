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

import better.files.StringOps
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{ Schema, SchemaFactory }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try
import scala.xml.{ Elem, PrettyPrinter }

trait SchemedXml extends DebugEnhancedLogging {
  protected val schemaLocation: String // TODO property per object?
  protected val schemaNameSpace: String
  lazy val triedSchema: Try[Schema] = Try {
    // lazy for two reasons:
    // - only subclasses have a value for schemaLocation
    // - postpone loading until we actually start validating
    trace(schemaLocation)
    SchemaFactory
      .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      .newSchema(Array(new StreamSource(schemaLocation)).toArray[Source])
  }

  // pretty provides friendly trouble shooting for complex XML's
  protected val prettyPrinter: PrettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  def validate(xml: Elem): Try[Elem] = {
    val xmlString = prettyPrinter.format(xml)
    logger.trace(xmlString)
    triedSchema
      .map(_.newValidator().validate(new StreamSource(xmlString.inputStream)))
      .map(_ => xml)
  }
}
