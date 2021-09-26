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
package nl.knaw.dans.easy.deposit.servlets

import nl.knaw.dans.lib.logging.servlet.{ HeaderMapEntry, MaskedLogFormatter, MultiParamsEntry }
import org.scalatra.ScalatraBase

trait UndoMasking extends MaskedLogFormatter {
  this: ScalatraBase =>

  // to troubleshoot unit tests set the log level to trace in test/resource/logback.xml
  // move the change to another change list in git to prevent an accidental commit

  override protected def formatRemoteAddress(remoteAddress: String): String = remoteAddress

  override protected def formatHeader(header: HeaderMapEntry): HeaderMapEntry = header

  override def formatParameter(param: MultiParamsEntry): MultiParamsEntry = param

  override def formatResponseHeader(header: HeaderMapEntry): HeaderMapEntry = header
}
