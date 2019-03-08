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

import javax.xml.validation.Schema
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.lib.error._

import scala.util.{ Success, Try }

class FilesXmlSpec extends TestSupportFixture {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    (testDir / "data").createIfNotExists(asDirectory = true)
  }

  private lazy val triedSchema: Try[Schema] = FilesXml.loadSchema

  "apply" should "produce an empty xml" in {
    prettyPrinter.format(createFilesXml) shouldBe prettyPrinter.format(
      <files
        xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/files/ https://easy.dans.knaw.nl/schemas/bag/metadata/files/2018/04/files.xsd"
      >
      </files>
    )
  }

  it should "produce xml with different mime types" in {
    (testDir / "data" / "folder").createIfNotExists(asDirectory = true)
    (testDir / "data" / "test.txt").touch()
    (testDir / "data" / "folder" / "test.xml").touch()

    val xml = createFilesXml
    (xml \ "file").toList.sortBy(_.attribute("filepath").toString) shouldBe
        <file filepath="data/folder/test.xml">
          <dcterms:format>application/xml</dcterms:format>
        </file>
        <file filepath="data/test.txt">
          <dcterms:format>text/plain</dcterms:format>
        </file>
    assume(triedSchema.isAvailable)
    triedSchema.validate(xml) should matchPattern {
      case Success(_) =>
    }
  }

  private def createFilesXml = {
    FilesXml(testDir / "data").getOrRecover(e => fail(e.toString))
  }
}
