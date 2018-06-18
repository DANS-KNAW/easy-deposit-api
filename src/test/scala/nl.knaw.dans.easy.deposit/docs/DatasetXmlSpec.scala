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
import javax.xml.validation.{ Schema, SchemaFactory }
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ Author, DateQualifier, QualifiedSchemedValue, SchemedKeyValue }
import nl.knaw.dans.lib.error._
import resource.Using

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node, PrettyPrinter }

class DatasetXmlSpec extends TestSupportFixture with DdmBehavior {

  private val minimal = DatasetMetadata(
    """{
      |  "titles": [""],
      |  "descriptions": [""],
      |  "dates": [
      |    { "scheme": "dcterms:W3CDTF", "value": "2018", "qualifier": "dcterms:created" },
      |    { "scheme": "dcterms:W3CDTF", "value": "2018", "qualifier": "dcterms:available" },
      |  ],
      |  "creators": [ { "initials": "", "surname": "Foo" } ],
      |  "accessRights": { "category": "OPEN_ACCESS" },
      |  "audiences": [ { "scheme": "", "key": "D35200", "value": ""} ]
      |}""".stripMargin).getOrElse(fail("parsing minimal json failed"))

  /** provides the verbose namespaces for inline DDM */
  override val emptyDDM: Elem = DatasetXml(minimal)
    .recoverWith { case e => fail(e) }
    .getOrElse(fail("recovering from preparation errors failed"))
    .copy(child = Seq())

  "minimal" should behave like validDatasetMetadata(
    input = Success(minimal),
    expectedOutput = Seq(
      <ddm:profile>
        <dc:title></dc:title>
        <dcterms:description></dcterms:description>
        <dcx-dai:creatorDetails>
          <dcx-dai:author>
            <dcx-dai:initials></dcx-dai:initials>
            <dcx-dai:surname>Foo</dcx-dai:surname>
          </dcx-dai:author>
        </dcx-dai:creatorDetails>
        <ddm:created>2018</ddm:created>
        <ddm:available>2018</ddm:available>
        <ddm:audience>D35200</ddm:audience>
        <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
      </ddm:profile>,
      <ddm:dcmiMetadata>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
    )
  )

  "minimal with multiple titles" should behave like validDatasetMetadata(
    input = Success(minimal.copy(titles = Some(Seq("Lorum", "ipsum")))),
    subset = { actualDDM => emptyDDM.copy(child = <ddm:profile>{actualDDM \ "profile" \ "title"}</ddm:profile>) },
    expectedOutput = Seq(
      <ddm:profile>
        <dc:title>Lorum</dc:title>
        <dc:title>ipsum</dc:title>
      </ddm:profile>
    )
  )

  "minimal with rightsHolders" should behave like {
    val someCreators = Some(Seq(
      Author(
        initials = Some("F.O.O."),
        surname = Some("Bar")
      ),
      Author(
        initials = Some("O."),
        surname = Some("Belix"),
        role = Some(SchemedKeyValue("", "RightsHolder", ""))
      )
    ))
    val someContributors = Some(Seq(
      Author(
        initials = Some("A.S."),
        surname = Some("Terix"),
        role = Some(SchemedKeyValue("", "RightsHolder", ""))
      )
    ))
    validDatasetMetadata(
      input = Success(minimal.copy(creators = someCreators, contributors = someContributors)),
      subset = { actualDDM => emptyDDM.copy(child = actualDDM \ "dcmiMetadata") },
      expectedOutput = Seq(
        //N.B: creators in ddm:profile unless they are rightsHolders
        <ddm:dcmiMetadata>
        <dcterms:rightsHolder>A.S. Terix</dcterms:rightsHolder>
        <dcterms:rightsHolder>O. Belix</dcterms:rightsHolder>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
    )
    )
  }

  "minimal with all types of dates" should behave like {
    val date = "2018-06-14"
    val dates = DateQualifier.values.toSeq.map { q => DatasetMetadata.Date(date, q) }
    val someDates = Some( // one type of date twice with different precision
      dates :+ DatasetMetadata.Date("2018-01", DateQualifier.modified)
    )
    validDatasetMetadata(
      input = Success(minimal.copy(dates = someDates)),
      subset = { actualDDM => emptyDDM.copy(child = actualDDM \ "dcmiMetadata") },
      expectedOutput = Seq(
        // the dateSubmitted specified above is replaced by "now" as set by the fixture
        // dateCreated and dateAvailable are documented with the pure minimal test
        <ddm:dcmiMetadata>
          <dc:date xsi:type="dcterms:W3CDTF">{ date }</dc:date>
          <dcterms:dateAccepted xsi:type="dcterms:W3CDTF">{ date }</dcterms:dateAccepted>
          <dcterms:dateCopyrighted xsi:type="dcterms:W3CDTF">{ date }</dcterms:dateCopyrighted>
          <dcterms:issued xsi:type="dcterms:W3CDTF">{ date }</dcterms:issued>
          <dcterms:modified xsi:type="dcterms:W3CDTF">{ date }</dcterms:modified>
          <dcterms:valid xsi:type="dcterms:W3CDTF">{ date }</dcterms:valid>
          <dcterms:modified xsi:type="dcterms:W3CDTF">2018-01</dcterms:modified>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        </ddm:dcmiMetadata>
      )
    )
  }

  "RichElem.setTag" should "report an error" in {
    import DatasetXml.RichElem
    val source = new QualifiedSchemedValue[String, String](None, "", "a:b:c")
    Try {
      <key>Lorum Ipsum</key>.setTag(DatasetXml.targetFromQualifier, source)
    } should matchPattern {
      case Failure(e: Exception) if e.getMessage == "invalid DatasetMetadata: class java.lang.Exception expecting (label) or (prefix:label); got [a:b:c] to adjust the <key> of <key>Lorum Ipsum</key> created from: QualifiedSchemedValue(None,,a:b:c)" =>
    }
  }

  "apply" should "report a missing title" in {
    DatasetXml(minimal.copy(titles = None)) should matchPattern {
      case Failure(e) if e.getMessage == "no content for mandatory dc:title" =>
    }
  }

  it should "report an empty list of titles" in {
    DatasetXml(minimal.copy(titles = Some(Seq.empty))) should matchPattern {
      case Failure(e) if e.getMessage == "no content for mandatory dc:title" =>
    }
  }

  "datasetmetadata.json" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata.json")
  )
  // TODO keep resources in sync with UI module: https://github.com/DANS-KNAW/easy-deposit-ui/blob/784fdc5/src/test/typescript/mockserver/metadata.ts#L246
  "datasetmetadata-from-ui-some.json" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata-from-ui-some.json")
  )
  "datasetmetadata-from-ui-all.json without one of the authors" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata-from-ui-all.json"),
    expectedOutput = Seq(
      <ddm:profile>
        <dc:title xml:lang="nld">title 1</dc:title>
        <dc:title xml:lang="nld">title2</dc:title>
        <dcterms:description xml:lang="nld">description1</dcterms:description>
        <dcterms:description xml:lang="nld">description2</dcterms:description>
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
      </ddm:profile>,
      <ddm:dcmiMetadata>
        <dcterms:alternative xml:lang="nld">alternative title 1</dcterms:alternative>
        <dcterms:alternative xml:lang="nld">alternative title2</dcterms:alternative>
        <dcx-dai:contributorDetails>
          <dcx-dai:author>
            <dcx-dai:titles xml:lang="nld">Dr.</dcx-dai:titles>
            <dcx-dai:initials>O.</dcx-dai:initials>
            <dcx-dai:insertions>van</dcx-dai:insertions>
            <dcx-dai:surname>Belix</dcx-dai:surname>
          </dcx-dai:author>
        </dcx-dai:contributorDetails>
        <dcx-dai:contributorDetails>
          <dcx-dai:organization>
            <dcx-dai:name xml:lang="nld">my organization</dcx-dai:name>
          </dcx-dai:organization>
        </dcx-dai:contributorDetails>
        <dcterms:rightsHolder>rightsHolder1</dcterms:rightsHolder>
        <dcterms:rightsHolder>Dr. A.S. van Terix</dcterms:rightsHolder>
        <dcterms:publisher xml:lang="nld">pub1</dcterms:publisher>
        <dcterms:publisher xml:lang="nld">pub2</dcterms:publisher>
        <dc:source xml:lang="nld">source1</dc:source>
        <dc:source xml:lang="nld">source2</dc:source>
        <dcterms:dateCopyrighted xsi:type="dcterms:W3CDTF">2018-03-18</dcterms:dateCopyrighted>
        <dcterms:valid xsi:type="dcterms:W3CDTF">2018-03-17</dcterms:valid>
        <dcterms:modified>2018-02-02</dcterms:modified>
        <dcterms:issued>Groundhog day</dcterms:issued>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        <dcterms:license>http://creativecommons.org/publicdomain/zero/1.0</dcterms:license>
      </ddm:dcmiMetadata>
    )
  )

  private def parseTestResource(file: String) = Try {
    (File(getClass.getResource("/manual-test")) / file).contentAsString
  }.flatMap(DatasetMetadata(_))
}

trait DdmBehavior {
  this: TestSupportFixture =>

  val emptyDDM: Elem

  lazy val triedSchema: Try[Schema] = Try { // loading postponed until we actually start validating
    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    val schemas = "http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd" ::
      "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd" :: Nil
    factory.newSchema(schemas.map(xsd => new StreamSource(xsd)).toArray[Source])
  }

  // pretty provides friendlier trouble shooting for complex XML's than Utility.trim
  private val prettyPrinter: PrettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  /**
   * @param input          to be converted to DDM
   * @param subset         subset of generated DDM that should match expectedOutput
   * @param expectedOutput members of <ddm:DDM> that should equal
   *                       the members of subset(DatsetXm(input.get))
   */
  def validDatasetMetadata(input: Try[DatasetMetadata],
                           subset: Elem => Elem = identity,
                           expectedOutput: Seq[Node] = Seq.empty
                          ): Unit = {
    lazy val datasetMetadata = input
      .doIfFailure { case e => println(s"$e") }
      .getOrElse(fail("test input should be a success"))
    lazy val ddm = DatasetXml(datasetMetadata)
      .doIfFailure { case e => println(s"$e") }
      .getOrElse(fail("can't create DDM from test input"))

    if (expectedOutput.nonEmpty) it should "generate expected DDM" in {
      prettyPrinter.format(subset(ddm)) shouldBe
        prettyPrinter.format(emptyDDM.copy(child = expectedOutput))
    }

    it should "generate valid DDM" in {
      assume(triedSchema.isSuccess)
      val validator = triedSchema.getOrElse(fail("no schema available despite assume")).newValidator()
      val xmlString = prettyPrinter.format(ddm)
      Using.bufferedInputStream(new ByteArrayInputStream(
        xmlString.getBytes(StandardCharsets.UTF_8)
      )).map(inputStream =>
        validator.validate(new StreamSource(inputStream))
      ).tried // println to troubleshoot a failing test: the error message shows a line number
        .doIfFailure { case _ => println(xmlString) } shouldBe a[Success[_]]
    }
  }
}
