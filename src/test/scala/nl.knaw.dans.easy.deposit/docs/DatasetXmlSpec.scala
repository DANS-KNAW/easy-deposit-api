package nl.knaw.dans.easy.deposit.docs

import better.files.File
import nl.knaw.dans.easy.deposit.TestSupportFixture

class DatasetXmlSpec extends TestSupportFixture {

  val prettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  "apply" should "produce a minimal DDM" in {
    val datasetMetadata = DatasetMetadata((File("src") / "test" / "resources" / "manual-test" / "datasetmetadata-from-ui-all.json").contentAsString)
      .getOrElse(fail("preparing a valid json failed"))
    val expected =
      <ddm:DDM
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:dct="http://purl.org/dc/terms/"
        xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
        xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
        xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd"
      >
        <ddm:profile>
          <dct:title>title 1</dct:title>
          <dct:title>title2</dct:title>
          <dct:alternative>alternative title 1</dct:alternative>
          <dct:alternative>alternative title2</dct:alternative>
          <dc:description>description1</dc:description>
          <dc:description>description2</dc:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:titles>Drs.</dcx-dai:titles>
              <dcx-dai:initials>D.A.</dcx-dai:initials>
              <dcx-dai:insertions></dcx-dai:insertions>
              <dcx-dai:surname>NS</dcx-dai:surname>
              <dcx-dai:role scheme="datacite:contributorType">ContactPerson</dcx-dai:role>
              <dcx-dai:organization>
                <dcx-dai:name>KNAW</dcx-dai:name>
              </dcx-dai:organization></dcx-dai:author>
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
            <dcx-dai:titles>Dr.</dcx-dai:titles>
            <dcx-dai:initials>O.</dcx-dai:initials>
            <dcx-dai:insertions>van</dcx-dai:insertions>
            <dcx-dai:surname>Belix</dcx-dai:surname>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
            <dcx-dai:organization>
                  <dcx-dai:name>my organization</dcx-dai:name>
            </dcx-dai:organization>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcterms:rightsholder>
            <dcx-dai:author>
              <dcx-dai:role scheme="datacite:contributorType">RightsHolder</dcx-dai:role>
              <dcx-dai:organization>
                <dcx-dai:name>rightsHolder1</dcx-dai:name>
              </dcx-dai:organization>
            </dcx-dai:author>
          </dcterms:rightsholder>
          <dcterms:rightsholder>
            <dcx-dai:author>
              <dcx-dai:titles>Dr.</dcx-dai:titles>
              <dcx-dai:initials>A.S.</dcx-dai:initials>
              <dcx-dai:insertions>van</dcx-dai:insertions>
              <dcx-dai:surname>Terix</dcx-dai:surname>
              <dcx-dai:role scheme="datacite:contributorType">RightsHolder</dcx-dai:role>
            </dcx-dai:author>
          </dcterms:rightsholder>
          <dcterms:dateCopyrighted scheme="dcterms:W3CDTF">2018-03-18</dcterms:dateCopyrighted>
          <dcterms:valid scheme="dcterms:W3CDTF">2018-03-17</dcterms:valid>
          <dcterms:modified>2018-02-02</dcterms:modified>
          <dcterms:issued>Groundhog day</dcterms:issued>
          <dct:license>http://creativecommons.org/publicdomain/zero/1.0</dct:license>
        </ddm:dcmiMetadata>
        <ddm:additional-xml>
        </ddm:additional-xml>
      </ddm:DDM>
    val actual = DatasetXml(datasetMetadata).getOrElse(fail("conversion failed"))
    println(prettyPrinter.format(actual))
    prettyPrinter.format(actual) shouldBe prettyPrinter.format(expected)
  }
}
