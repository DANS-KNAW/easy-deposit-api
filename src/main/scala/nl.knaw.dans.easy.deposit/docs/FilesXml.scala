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

import java.nio.file.Path

import better.files.File
import org.apache.tika.Tika

import scala.util.Try
import scala.xml.Elem

object FilesXml {

  /**
   * Generates files.xml from files in draft bag
   *
   * @return Elem
   */
  def apply(pathData: File): Try[Elem] = Try {

    val tika = new Tika
    val files = pathData
      .walk()
      .filter(_.isRegularFile)
      .map { file =>
        val fileName = pathData.relativize(file).toString
        <file filepath={fileName}>
          <dcterms:format>{tika.detect(fileName)}</dcterms:format>
        </file>
      }

    <files xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://purl.org/dc/terms/ http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd http://easy.dans.knaw.nl/schemas/bag/metadata/files/ http://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd">
      {files}
    </files>
  }
}
