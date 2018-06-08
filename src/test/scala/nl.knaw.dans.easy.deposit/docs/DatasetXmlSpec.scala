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

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import better.files.File
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import nl.knaw.dans.easy.deposit.TestSupportFixture
import resource.Using

import scala.util.{ Failure, Success, Try }

class DatasetXmlSpec extends TestSupportFixture {
  mockDateTimeNow("2018-06-08T21:43:01.576")

  private val triedSchema = Try {
    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    val schemas = "http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd" ::
      "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd" :: Nil
    factory.newSchema(schemas.map(xsd => new StreamSource(xsd)).toArray[Source])
  }
  private val prettyPrinter = new scala.xml.PrettyPrinter(1024, 2)
  private val minimal = DatasetMetadata(
    """{
      |  "titles": [""],
      |  "descriptions": [""],
      |  "dates": [
      |    { "scheme": "dcterms:W3CDTF", "value": "2018", "qualifier": "dcterms:created" },
      |    { "scheme": "dcterms:W3CDTF", "value": "2018", "qualifier": "dcterms:available" },
      |  ],
      |  "creators": [ { "initials": "", "surname": "" } ],
      |  "accessRights": { "category": "OPEN_ACCESS" },
      |  "audiences": [ { "scheme": "", "key": "D35200", "value": ""} ]
      |}""".stripMargin).getOrElse(fail("parsing minimal json failed"))

  "apply" should "produce DDM from  minimal json" in {
    DatasetXml(minimal) shouldBe a[Success[_]]
  }

  it should "report a missing title" in {
    DatasetXml(minimal.copy(titles = None)) should matchPattern {
      case Failure(e) if e.getMessage == "no content for mandatory dcterms:title" =>
    }
  }

  it should "report a missing description" in {
    DatasetXml(minimal.copy(descriptions = None)) should matchPattern {
      case Failure(e) if e.getMessage == "no content for mandatory dc:description" =>
    }
  }
  // TODO further variations on minimal should test the specifications line by line

  "datasetmetadata-from-ui-all.json" should "produce expected DDM" in {
    val expected =
      <ddm:DDM
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:dcterms="http://purl.org/dc/terms/"
        xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
        xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
        xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
      >
        <ddm:profile>
          <dcterms:title xml:lang="nld">title 1</dcterms:title>
          <dcterms:title xml:lang="nld">title2</dcterms:title>
          <dc:description xml:lang="nld">description1</dc:description>
          <dc:description xml:lang="nld">description2</dc:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:titles xml:lang="nld">Drs.</dcx-dai:titles>
              <dcx-dai:initials>D.A.</dcx-dai:initials>
              <dcx-dai:insertions></dcx-dai:insertions>
              <dcx-dai:surname>NS</dcx-dai:surname>
              <dcx-dai:role>ContactPerson</dcx-dai:role>
              <dcx-dai:organization>
                <dcx-dai:name xml:lang="nld">KNAW</dcx-dai:name>
              </dcx-dai:organization>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:initials>Foo</dcx-dai:initials>
              <dcx-dai:insertions>van</dcx-dai:insertions>
              <dcx-dai:surname>Bar</dcx-dai:surname>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <ddm:created>2018-03-19</ddm:created>
          <ddm:available>2018-03-14</ddm:available>
          <ddm:audience>D35200</ddm:audience>
          <ddm:audience>D33000</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dcterms:alternative xml:lang="nld">alternative title 1</dcterms:alternative>
          <dcterms:alternative xml:lang="nld">alternative title2</dcterms:alternative>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:titles xml:lang="nld">Dr.</dcx-dai:titles>
              <dcx-dai:initials>O.</dcx-dai:initials>
              <dcx-dai:insertions>van</dcx-dai:insertions>
              <dcx-dai:surname>Belix</dcx-dai:surname>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:organization>
                <dcx-dai:name xml:lang="nld">my organization</dcx-dai:name>
              </dcx-dai:organization>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcterms:rightsholder>
            <dcx-dai:author>
              <dcx-dai:role>RightsHolder</dcx-dai:role>
              <dcx-dai:organization>
                <dcx-dai:name xml:lang="nld">rightsHolder1</dcx-dai:name>
              </dcx-dai:organization>
            </dcx-dai:author>
          </dcterms:rightsholder>
          <dcterms:rightsholder>
            <dcx-dai:author>
              <dcx-dai:titles xml:lang="nld">Dr.</dcx-dai:titles>
              <dcx-dai:initials>A.S.</dcx-dai:initials>
              <dcx-dai:insertions>van</dcx-dai:insertions>
              <dcx-dai:surname>Terix</dcx-dai:surname>
              <dcx-dai:role>RightsHolder</dcx-dai:role>
            </dcx-dai:author>
          </dcterms:rightsholder>
          <dcterms:publisher xml:lang="nld">pub1</dcterms:publisher>
          <dcterms:publisher xml:lang="nld">pub2</dcterms:publisher>
          <dc:source xml:lang="nld">source1</dc:source>
          <dc:source xml:lang="nld">source2</dc:source>
          <dcterms:dateCopyrighted>2018-03-18</dcterms:dateCopyrighted>
          <dcterms:valid>2018-03-17</dcterms:valid>
          <dcterms:modified>2018-02-02</dcterms:modified>
          <dcterms:issued>Groundhog day</dcterms:issued>
          <dcterms:dateSubmitted>2018-06-08</dcterms:dateSubmitted>
          <dcterms:license>http://creativecommons.org/publicdomain/zero/1.0</dcterms:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>

    val datasetMetadata = DatasetMetadata(readTestData("datasetmetadata-from-ui-all.json"))
      .getOrElse(fail("could not parse test data"))
    val actual = DatasetXml(datasetMetadata).getOrElse(fail("conversion to DDM failed"))

    prettyPrinter.format(actual) shouldBe prettyPrinter.format(expected)
  }

  "DDM validation" should "succeed for the minimal json" in {
    assume(triedSchema.isSuccess)
    convertAndValidate(minimal) shouldBe a[Success[_]]
  }

  it should "succeed for datasetmetadata-from-ui-all.json" in {
    assume(triedSchema.isSuccess)
    parseConvertAndValidate(readTestData("datasetmetadata-from-ui-all.json")) shouldBe a[Success[_]]
  }

  private def parseConvertAndValidate(jsonString: String) = {
    val datasetMetadata = DatasetMetadata(jsonString)
      .getOrElse(fail("could not parse test data"))
    convertAndValidate(datasetMetadata)
  }

  private def convertAndValidate(datasetMetadata: DatasetMetadata) = {
    val xmlString = prettyPrinter.format(
      DatasetXml(datasetMetadata)
        .getOrElse(fail("conversion to DDM failed"))
    )
    val validator = triedSchema.getOrElse(fail("no schema available")).newValidator()
    val inputStream = new ByteArrayInputStream(xmlString.getBytes(StandardCharsets.UTF_8))
    Using.bufferedInputStream(inputStream)
      .map(inputStream => validator.validate(new StreamSource(inputStream)))
      .tried
      .recoverWith { case e =>
        println(xmlString)
        Failure(e)
      }
  }

  private def readTestData(value: String) = {
    Try {
      (File(getClass.getResource("/manual-test")) / value).contentAsString
    }.getOrElse(fail("could not read test data"))
  }
}
