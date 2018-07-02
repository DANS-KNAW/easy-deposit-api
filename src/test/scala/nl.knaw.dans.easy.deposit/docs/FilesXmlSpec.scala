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

import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.lib.error._

import scala.xml.PrettyPrinter

class FilesXmlSpec extends TestSupportFixture {
  // pretty provides friendly trouble shooting for complex XML's
  private val prettyPrinter: PrettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "apply" should "produce an empty xml" in {
    prettyPrinter.format(createFilesXml) shouldBe prettyPrinter.format(
      <files
        xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://purl.org/dc/terms/ http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd http://easy.dans.knaw.nl/schemas/bag/metadata/files/ http://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd"
      >
      </files>
    )
  }

  it should "produce xml with different mime types" in {
    (testDir / "folder").createIfNotExists(asDirectory = true)

    (testDir / "test.txt").touch()
    (testDir / "folder" / "test.xml").touch()

    (createFilesXml \ "file").toList.sortBy(_.attribute("filepath").toString) shouldBe
        <file filepath="folder/test.xml">
          <dcterms:format>application/xml</dcterms:format>
        </file>
        <file filepath="test.txt">
          <dcterms:format>text/plain</dcterms:format>
        </file>
  }

  private def createFilesXml = {
    FilesXml(testDir).getOrRecover(e => fail(e))
  }
}
