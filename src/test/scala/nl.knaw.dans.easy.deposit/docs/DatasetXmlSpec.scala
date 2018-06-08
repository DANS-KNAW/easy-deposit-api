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

import better.files.File
import nl.knaw.dans.easy.deposit.TestSupportFixture

import scala.util.{ Failure, Success, Try }

class DatasetXmlSpec extends TestSupportFixture {

  private val prettyPrinter = new scala.xml.PrettyPrinter(1024, 2)
  private val minimal = DatasetMetadata(
    """{
      |  "titles": [""],
      |  "descriptions": [""],
      |  "dates": [
      |    { "scheme": "", "value": "", "qualifier": "dcterms:created" },
      |    { "scheme": "", "value": "", "qualifier": "dcterms:available" },
      |  ],
      |  "creators": [ { "initials": "", "surname": "" } ],
      |  "accessRights": { "category": "OPEN_ACCESS" },
      |  "audiences": [ { "scheme": "", "key": "", "value": ""} ]
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
          <dcterms:alternative xml:lang="nld">alternative title 1</dcterms:alternative>
          <dcterms:alternative xml:lang="nld">alternative title2</dcterms:alternative>
          <dc:description xml:lang="nld">description1</dc:description>
          <dc:description xml:lang="nld">description2</dc:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:titles xml:lang="nld">Drs.</dcx-dai:titles>
              <dcx-dai:initials>D.A.</dcx-dai:initials>
              <dcx-dai:insertions></dcx-dai:insertions>
              <dcx-dai:surname>NS</dcx-dai:surname>
              <dcx-dai:role scheme="datacite:contributorType">ContactPerson</dcx-dai:role>
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
          <ddm:created scheme="dcterms:W3CDTF">2018-03-19</ddm:created>
          <ddm:available scheme="dcterms:W3CDTF">2018-03-14</ddm:available>
          <ddm:audience scheme="narcis:DisciplineType">D35200</ddm:audience>
          <ddm:audience scheme="narcis:DisciplineType">D33000</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
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
              <dcx-dai:role scheme="datacite:contributorType">RightsHolder</dcx-dai:role>
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
              <dcx-dai:role scheme="datacite:contributorType">RightsHolder</dcx-dai:role>
            </dcx-dai:author>
          </dcterms:rightsholder>
          <dcterms:publisher xml:lang="nld">pub1</dcterms:publisher>
          <dcterms:publisher xml:lang="nld">pub2</dcterms:publisher>
          <dc:source xml:lang="nld">source1</dc:source>
          <dc:source xml:lang="nld">source2</dc:source>
          <dcterms:dateCopyrighted scheme="dcterms:W3CDTF">2018-03-18</dcterms:dateCopyrighted>
          <dcterms:valid scheme="dcterms:W3CDTF">2018-03-17</dcterms:valid>
          <dcterms:modified>2018-02-02</dcterms:modified>
          <dcterms:issued>Groundhog day</dcterms:issued>
          <dcterms:license>http://creativecommons.org/publicdomain/zero/1.0</dcterms:license>
        </ddm:dcmiMetadata>
        <ddm:additional-xml>
        </ddm:additional-xml>
      </ddm:DDM>

    val json = Try {
      (File("src") / "test" / "resources" / "manual-test" / "datasetmetadata-from-ui-all.json").contentAsString
    }.getOrElse(fail("could not read test data"))
    val datasetMetadata = DatasetMetadata(json).getOrElse(fail("could not parse test data"))
    val actual = DatasetXml(datasetMetadata).getOrElse(fail("conversion to DDM failed"))

    prettyPrinter.format(actual) shouldBe prettyPrinter.format(expected)
  }
}
