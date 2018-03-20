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

import better.files._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.{ DefaultFormats, Formats }
import org.scalatra.ScalatraServlet
import org.scalatra.json._

import scala.language.existentials


class DepositServlet(app: EasyDepositApiApp) extends ScalatraServlet
  with JacksonJsonSupport
  with DebugEnhancedLogging {

  // Sets up automatic case class to JSON output serialization
  protected implicit lazy val jsonFormats: Formats = DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all ++
    org.json4s.ext.JavaTypesSerializers.all

  // Before every action runs, set the content type to be in JSON format.
  before() {
    contentType = formats("json")
  }

  post("/") {

    val dir: File = "./data/deposit_test"
      .toFile
      .createIfNotExists(asDirectory = true, createParents = true)

    println(DepositDir.create(dir, "user007"))

  }
}
