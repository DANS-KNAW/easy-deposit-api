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
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ DateQualifier, QualifiedSchemedValue }
import resource.Using

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node, PrettyPrinter, Utility }

trait DdmBehavior {
  this: TestSupportFixture =>

  lazy val triedSchema: Try[Schema] = Try { // loading postponed until we actually start validating
    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    val schemas = "http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd" ::
      "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd" :: Nil
    factory.newSchema(schemas.map(xsd => new StreamSource(xsd)).toArray[Source])
  }
  val emptyDDM: Elem

  // pretty provides friendlier trouble shooting for complex XML's than Utility.trim
  private val prettyPrinter: PrettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  /**
   * @param input          a Success(DatasetMetadata)
   * @param expectedOutput <ddm:profile> and optionaly <ddm:dcmiMetadata>
   *                       if provided, input should convert into a DDM with the same content
   */
  def validDatasetMetadata(input: Try[DatasetMetadata], expectedOutput: Seq[Node]): Unit = {
    lazy val datasetMetadata = input.recoverWith { case e =>
      println(s"$e")
      Failure(e)
    }.getOrElse(fail("test input should be a success"))
    lazy val ddm = DatasetXml(datasetMetadata).recoverWith { case e =>
      println(s"$e")
      Failure(e)
    }.getOrElse(fail("can't create DDM from test input"))

    it should "generate expected DDM" in {
      // TODO change into "should contain": leafs in expected should occur with same path in ddm
      // perhaps with something similar to https://stackoverflow.com/questions/39001421/
      // or a third argument (with xpaths?) to extract subsets from the generated DDM
      val expected = emptyDDM.copy(child = expectedOutput)
      if(expectedOutput.nonEmpty)
        prettyPrinter.format(ddm) shouldBe
          prettyPrinter.format(expected)
      println(ddm.child diff expected.child mkString ", ")
    }

    it should "generate valid DDM" in {
      assume(triedSchema.isSuccess)
      val validator = triedSchema.getOrElse(fail("no schema available despite assume")).newValidator()
      val xmlString = prettyPrinter.format(ddm)
      Using.bufferedInputStream(new ByteArrayInputStream(
        xmlString.getBytes(StandardCharsets.UTF_8)
      )).map(inputStream =>
        validator.validate(new StreamSource(inputStream))
      ).tried
        .recoverWith { case e =>
          println(xmlString) // to trouble shoot reported line numbers
          Failure(e)
        } shouldBe a[Success[_]]
    }
  }
}

class DatasetXmlSpec extends TestSupportFixture with DdmBehavior {

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

  override val emptyDDM: Elem = toDDM(minimal).copy(child = Seq())

  "minimal" should behave like validDatasetMetadata(Success(minimal), Seq(
    <ddm:profile>
      <dcterms:title></dcterms:title>
      <dc:description></dc:description>
      <dcx-dai:creatorDetails>
        <dcx-dai:author>
          <dcx-dai:initials></dcx-dai:initials>
          <dcx-dai:surname></dcx-dai:surname>
        </dcx-dai:author>
      </dcx-dai:creatorDetails>
      <ddm:created>2018</ddm:created>
      <ddm:available>2018</ddm:available>
      <ddm:audience>D35200</ddm:audience>
      <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
    </ddm:profile>,
    <ddm:dcmiMetadata>
      <dcterms:dateSubmitted>2018-03-22</dcterms:dateSubmitted>
    </ddm:dcmiMetadata>
  ))

  "apply" should "report a missing title" in {
    DatasetXml(minimal.copy(titles = None)) should matchPattern {
      case Failure(e) if e.getMessage == "no content for mandatory dcterms:title" =>
    }
  }

  it should "report a missing description" in {
    DatasetXml(minimal.copy(descriptions = None)) should matchPattern {
      case Failure(e) if e.getMessage == "no content for mandatory dc:description" =>
    }
  }

  it should "produce multiple titles" in {
    val ddm = toDDM(minimal.copy(titles = Some(Seq("Lorum", "ipsum"))))
    (ddm \ "profile" \ "title").iterator.toList.map(Utility.trim) shouldBe
      List(
        <dcterms:title>Lorum</dcterms:title>,
        <dcterms:title>ipsum</dcterms:title>
      ).map(Utility.trim)
  }

  it should "produce all types of dates" in {
    val date = "2018-06-14"
    val dates = DateQualifier.values.toSeq.map { q => DatasetMetadata.Date(date, q) } :+
      DatasetMetadata.Date("2018-01-01", DateQualifier.modified) // one date twice (different values)

    val ddm = toDDM(minimal.copy(dates = Some(dates)))
    (ddm \ "dcmiMetadata" \ "date").text shouldBe date
    (ddm \ "dcmiMetadata" \ "dateAccepted").text shouldBe date
    (ddm \ "dcmiMetadata" \ "dateCopyrighted").text shouldBe date
    (ddm \ "dcmiMetadata" \ "issued").text shouldBe date
    (ddm \ "dcmiMetadata" \ "modified").text shouldBe date + "2018-01-01"
    (ddm \ "dcmiMetadata" \ "valid").text shouldBe date

    // specified value overwritten with a generated value: "now" as set by fixture
    (ddm \ "dcmiMetadata" \ "dateSubmitted").text shouldBe "2018-03-22"

    // ddm:created and ddm:available are generated in ddm:profile as shown with plain minimal test
  }
  // TODO further variations on minimal should test the specifications line by line

  "RichElem.setTag" should "report an error" in {
    // TODO had to make targetFromQualifier and RichElem public for this test
    // can't cause this error through the apply method
    // because it does not yet use targetFromQualifier for a non-enum
    import DatasetXml.RichElem
    val source = new QualifiedSchemedValue[String, String](None, "", "a:b:c")
    Try {
      <key>Lorum Ipsum</key>.setTag(DatasetXml.targetFromQualifier, source)
    } should matchPattern {
      case Failure(e: Exception) if e.getMessage == "invalid DatasetMetadata: class java.lang.Exception expecting (label) or (prefix:label); got [a:b:c] to adjust the <key> of <key>Lorum Ipsum</key> created from: QualifiedSchemedValue(None,,a:b:c)" =>
    }
  }

  "variations on minimal" should behave like validDatasetMetadata(
    {
      val dates = DateQualifier.values.toSeq.map { q => DatasetMetadata.Date("2018", q) } :+
        DatasetMetadata.Date("2017", DateQualifier.modified)
      Success(minimal.copy(
        titles = Some(Seq("Lorum", "ipsum")),
        descriptions = Some(Seq("Lorum", "ipsum")),
        alternativeTitles = Some(Seq("Lorum", "ipsum")),
        sources = Some(Seq("Lorum", "ipsum")),
        dates = Some(dates),
        license = Some("rabarbera")
      ))
    }, Seq.empty
  )

  "datasetmetadata.json" should behave like validDatasetMetadata(
    parseTestResource("datasetmetadata.json"),
    Seq.empty
  )
  // TODO keep resources in sync with UI module: https://github.com/DANS-KNAW/easy-deposit-ui/blob/784fdc5/src/test/typescript/mockserver/metadata.ts#L246
  "datasetmetadata-from-ui-some.json" should behave like validDatasetMetadata(
    parseTestResource("datasetmetadata-from-ui-some.json"),
    Seq.empty
  )
  "datasetmetadata-from-ui-all.json without RightsHolders" should behave like validDatasetMetadata(
    {
      parseTestResource("datasetmetadata-from-ui-all.json").map { metadata =>
        val validContributors = metadata.contributors.map(_.filter(_ match {
          case author if author.organization.getOrElse("") == "my organization" => false
          case author if author.organization.getOrElse("") == "rightsHolder1" => false
          case author if author.surname.getOrElse("") == "Terix" => false
          case _ => true
        }))
        metadata.copy(contributors = validContributors)
      }
    }, Seq(
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
      </ddm:profile>,
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
        <dcterms:publisher xml:lang="nld">pub1</dcterms:publisher>
        <dcterms:publisher xml:lang="nld">pub2</dcterms:publisher>
        <dc:source xml:lang="nld">source1</dc:source>
        <dc:source xml:lang="nld">source2</dc:source>
        <dcterms:dateCopyrighted>2018-03-18</dcterms:dateCopyrighted>
        <dcterms:valid>2018-03-17</dcterms:valid>
        <dcterms:modified>2018-02-02</dcterms:modified>
        <dcterms:issued>Groundhog day</dcterms:issued>
        <dcterms:dateSubmitted>2018-03-22</dcterms:dateSubmitted>
        <dcterms:license>http://creativecommons.org/publicdomain/zero/1.0</dcterms:license>
      </ddm:dcmiMetadata>
    )
  )

  private def toDDM(variant: DatasetMetadata) = {
    DatasetXml(variant).recoverWith { case e => fail(e) }.getOrElse(fail("recovering from preparation errors failed"))
  }

  private def parseTestResource(file: String) = Try {
    (File(getClass.getResource("/manual-test")) / file).contentAsString
  }.flatMap(DatasetMetadata(_))
}
