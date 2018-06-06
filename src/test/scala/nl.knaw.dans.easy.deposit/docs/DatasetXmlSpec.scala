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
        xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      >
        <ddm:profile>
          <dc:title>title 1</dc:title>
          <dc:title>title2</dc:title>
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
                <dcx-dai:name xml:lang="en">KNAW</dcx-dai:name>
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
      </ddm:DDM>
    val actual = DatasetXml(datasetMetadata).getOrElse(fail("conversion failed"))
    println(actual)
    prettyPrinter.format(actual) shouldBe prettyPrinter.format(expected)
  }
}
