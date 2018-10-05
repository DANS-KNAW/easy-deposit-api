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

import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{ Schema, SchemaFactory }
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ SchemedKeyValue, SchemedValue }
import nl.knaw.dans.easy.deposit.docs.JsonUtil.InvalidDocumentException
import nl.knaw.dans.easy.deposit.docs.dm.DateScheme.W3CDTF
import nl.knaw.dans.easy.deposit.docs.dm._
import nl.knaw.dans.lib.error._

import scala.util.{ Failure, Success, Try }
import scala.xml._

class DDMSpec extends TestSupportFixture with DdmBehavior {

  private val minimal = DatasetMetadata(minimalJsonString)
    .getOrRecover(fail(_))

  /** provides the verbose namespaces for inline DDM */
  lazy override val emptyDDM: Elem = DDM(minimal)
    .getOrRecover(fail(_))
    .copy(child = Seq())

  "minimal" should behave like validDatasetMetadata(
    input = Success(minimal),
    expectedDdmContent =
      <ddm:profile>
        <dc:title>Lorum ipsum</dc:title>
        <dcterms:description>dolor</dcterms:description>
        <dcx-dai:creatorDetails>
          <dcx-dai:author>
            <dcx-dai:initials>B.A.R.</dcx-dai:initials>
            <dcx-dai:surname>Foo</dcx-dai:surname>
          </dcx-dai:author>
        </dcx-dai:creatorDetails>
        <ddm:created>2018</ddm:created>
        <ddm:available>2018</ddm:available>
        <ddm:audience>D35200</ddm:audience>
        <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
      </ddm:profile>
      <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
  )

  "minimal with multiple titles" should behave like validDatasetMetadata(
    input = Success(minimal.copy(titles = Some(Seq("Lorum", "ipsum")))),
    subset = actualDDM => profileElements(actualDDM, "title"),
    expectedDdmContent =
      <ddm:profile>
        <dc:title>Lorum</dc:title>
        <dc:title>ipsum</dc:title>
      </ddm:profile>
  )

  "minimal with multiple descriptions" should behave like validDatasetMetadata(
    input = Success(minimal.copy(descriptions = Some(Seq("Lorum", "ipsum")))),
    subset = actualDDM => profileElements(actualDDM, "description"),
    expectedDdmContent =
      <ddm:profile>
        <dcterms:description>Lorum</dcterms:description>
        <dcterms:description>ipsum</dcterms:description>
      </ddm:profile>
  )

  "minimal with multiple audiences" should behave like validDatasetMetadata(
    input = Success(minimal.copy(audiences = Some(Seq(
      SchemedKeyValue("blabla", "D11200", "Lorum"),
      SchemedKeyValue("blabla", "D32400", "ipsum")
    )))),
    subset = actualDDM => profileElements(actualDDM, "audience"),
    expectedDdmContent =
      <ddm:profile>
        <ddm:audience>D11200</ddm:audience>
        <ddm:audience>D32400</ddm:audience>
      </ddm:profile>
  )

  "minimal with multiple alternativeTitles" should behave like validDatasetMetadata(
    input = Success(minimal.copy(alternativeTitles = Some(Seq("Lorum", "ipsum")))),
    subset = actualDDM => dcmiMetadata(actualDDM),
    expectedDdmContent =
      <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcterms:alternative>Lorum</dcterms:alternative>
        <dcterms:alternative>ipsum</dcterms:alternative>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
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
        role = Some(SchemedKeyValue("blabla", "RightsHolder", "Lorum Ipsum"))
      )
    ))
    val someContributors = Some(Seq(
      Author(
        initials = Some("A.S."),
        surname = Some("Terix"),
        role = Some(SchemedKeyValue("blabla", "RightsHolder", "Lorum Ipsum"))
      )
    ))
    validDatasetMetadata(
      input = Success(minimal.copy(creators = someCreators, contributors = someContributors)),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent =
        //N.B: creators in ddm:profile unless they are rightsHolders
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcterms:rightsHolder>A.S. Terix</dcterms:rightsHolder>
          <dcterms:rightsHolder>O. Belix</dcterms:rightsHolder>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        </ddm:dcmiMetadata>
    )
  }

  "minimal with all types of dates (except submitted)" should behave like {
    val date = "2018-06-14"
    val dates = DateQualifier.values.toSeq
      .withFilter(_ != DateQualifier.dateSubmitted)
      .map { qualifier => Date(Some(W3CDTF.toString), date, qualifier) }
    validDatasetMetadata(
      input = Success(minimal.copy(dates = Some(dates))),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent =
        // the dateSubmitted specified above is replaced by "now" as set by the fixture
        // dateCreated and dateAvailable are documented with the pure minimal test
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dc:date xsi:type="dcterms:W3CDTF">{ date }</dc:date>
          <dcterms:dateAccepted xsi:type="dcterms:W3CDTF">{ date }</dcterms:dateAccepted>
          <dcterms:dateCopyrighted xsi:type="dcterms:W3CDTF">{ date }</dcterms:dateCopyrighted>
          <dcterms:issued xsi:type="dcterms:W3CDTF">{ date }</dcterms:issued>
          <dcterms:modified xsi:type="dcterms:W3CDTF">{ date }</dcterms:modified>
          <dcterms:valid xsi:type="dcterms:W3CDTF">{ date }</dcterms:valid>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        </ddm:dcmiMetadata>
    )
  }

  "minimal with various types of dates" should behave like {
    // with and without qualifier, varying precision
    val json =
      """{  "dates": [
        |    { "value": "2018", "qualifier": "dcterms:created" },
        |    { "value": "2018", "qualifier": "dcterms:available" },
        |    { "value": "Groundhog day", "qualifier": "dcterms:dateAccepted" }
        |    { "value": "Groundhog day", "qualifier": "dcterms:dateCopyrighted" }
        |    { "value": "Groundhog day", "qualifier": "dcterms:issued" }
        |    { "value": "Groundhog day", "qualifier": "dcterms:modified" }
        |    { "qualifier": "dcterms:valid", "value": "Groundhog day" }
        |    { "qualifier": "dcterms:valid", "value": "2018"   , "scheme": "dcterms:W3CDTF" }
        |    { "qualifier": "dcterms:valid", "value": "2018-12", "scheme": "dcterms:W3CDTF" }
        |  ]
        |}
        |""".stripMargin
    validDatasetMetadata(
      input = DatasetMetadata(json).map(dm => minimal.copy(dates = dm.dates)),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent =
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcterms:dateAccepted>Groundhog day</dcterms:dateAccepted>
          <dcterms:dateCopyrighted>Groundhog day</dcterms:dateCopyrighted>
          <dcterms:issued>Groundhog day</dcterms:issued>
          <dcterms:modified>Groundhog day</dcterms:modified>
          <dcterms:valid>Groundhog day</dcterms:valid>
          <dcterms:valid xsi:type="dcterms:W3CDTF">2018</dcterms:valid>
          <dcterms:valid xsi:type="dcterms:W3CDTF">2018-12</dcterms:valid>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">{ nowYMD }</dcterms:dateSubmitted>
        </ddm:dcmiMetadata>
    )
  }

  "minimal with points and boxes of issue 1538" should behave like {
    val json =
      """  "spatialPoints": [
        |    { "scheme": "RD", "x": 79500, "y": 446750 },
        |    { "scheme": "degrees", "x": 52.0811, "y": 4.34521 }
        |  ],
        |  "spatialBoxes": [{
        |    "scheme": "RD",
        |    "north": 486890.5,
        |    "east": 121811.88,
        |    "south": 436172.5,
        |    "west": 91232.016
        |  }],
        |""".stripMargin
    validDatasetMetadata(
      input = minimalWith(json),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent =
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">{ nowYMD }</dcterms:dateSubmitted>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
              <Point xmlns="http://www.opengis.net/gml">
                  <pos>79500 446750</pos>
              </Point>
          </dcx-gml:spatial>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
              <Point xmlns="http://www.opengis.net/gml">
                  <pos>4.34521 52.0811</pos>
              </Point>
          </dcx-gml:spatial>
          <dcx-gml:spatial>
              <boundedBy xmlns="http://www.opengis.net/gml">
                  <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
                      <lowerCorner>91232.016 436172.5</lowerCorner>
                      <upperCorner>121811.88 486890.5</upperCorner>
                  </Envelope>
              </boundedBy>
          </dcx-gml:spatial>
        </ddm:dcmiMetadata>
    )
  }

  private def minimalWith(json: String) = {
    DatasetMetadata(minimalJsonString.replaceAll("^\\{", s"{$json"))
  }

  "minimal with points from split-multi-deposit" should behave like {
    val json =
      """  "spatialPoints":[
        |  { "x": 1, "y": 2, "scheme":"RD" },
        |  { "x": 3, "y": 4, "scheme":"http://some.example.com" },
        |  { "x": 5, "y": 6, "scheme":"degrees" },
        |]""".stripMargin
    validDatasetMetadata(
      input = minimalWith(json),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent =
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">{ nowYMD }</dcterms:dateSubmitted>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
            <Point xmlns="http://www.opengis.net/gml">
              <pos>1 2</pos>
            </Point>
          </dcx-gml:spatial>
          <dcx-gml:spatial srsName="http://some.example.com">
            <Point xmlns="http://www.opengis.net/gml">
              <pos>4 3</pos>
            </Point>
          </dcx-gml:spatial>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
            <Point xmlns="http://www.opengis.net/gml">
              <pos>6 5</pos>
            </Point>
          </dcx-gml:spatial>
        </ddm:dcmiMetadata>
    )
  }

  "minimal with boxes from split-multi-deposit" should behave like {
    val json =
      """  "spatialBoxes":[
        |  {"north": 1, "south": 2,  "east": 3,  "west": 4, "scheme":"RD"},
        |  {"north": 5, "south": 6,  "east": 7,  "west": 8, "scheme":"xyz"},
        |  {"north": 9, "south": 10, "east": 11, "west": 12, "scheme":"degrees"}
        |]""".stripMargin
    validDatasetMetadata(
      input = minimalWith(json),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent =
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">{ nowYMD }</dcterms:dateSubmitted>
          <dcx-gml:spatial>
            <boundedBy xmlns="http://www.opengis.net/gml">
              <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
                <lowerCorner>4 2</lowerCorner>
                <upperCorner>3 1</upperCorner>
              </Envelope>
            </boundedBy>
          </dcx-gml:spatial>
          <dcx-gml:spatial>
            <boundedBy xmlns="http://www.opengis.net/gml">
              <Envelope srsName="xyz">
                <lowerCorner>6 8</lowerCorner>
                <upperCorner>5 7</upperCorner>
              </Envelope>
            </boundedBy>
          </dcx-gml:spatial>
          <dcx-gml:spatial>
            <boundedBy xmlns="http://www.opengis.net/gml">
              <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
                <lowerCorner>10 12</lowerCorner>
                <upperCorner>9 11</upperCorner>
              </Envelope>
            </boundedBy>
          </dcx-gml:spatial>
        </ddm:dcmiMetadata>
    )
  }

  "RichElem.withLabel" should "report an error" in {
    import DDM.RichElem
    Try {
      <key>Lorum Ipsum</key>.withLabel("a:b:c")
    } should matchPattern {
      case Failure(InvalidDocumentException(_, e)) if e.getMessage ==
        "expecting (label) or (prefix:label); got [a:b:c] to adjust the <key> of <key>Lorum Ipsum</key>" =>
    }
  }

  "apply" should "report a missing title" in {
    DDM(minimal.copy(titles = None)) should matchPattern {
      case Failure(InvalidDocumentException(_, e)) if e.getMessage ==
        "Please set a title" =>
    }
  }

  it should "report an empty list of titles" in {
    DDM(minimal.copy(titles = Some(Seq.empty))) should matchPattern {
      case Failure(InvalidDocumentException(_, e)) if e.getMessage ==
        "Please set a title" =>
    }
  }

  it should "report an empty string as title" in {
    DDM(minimal.copy(titles = Some(Seq("   \t")))) should matchPattern {
      case Failure(InvalidDocumentException(_, e)) if e.getMessage ==
        "Please set a title" =>
    }
  }

  "issue-1538.json" should behave like validDatasetMetadata(
    input = parseTestResource("issue-1538.json").map(_.setDoi("mocked_DOI"))
  )

  "datasetmetadata.json" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata.json").map(_.setDoi("mocked_DOI"))
  )
  // TODO keep resources in sync with UI module: https://github.com/DANS-KNAW/easy-deposit-ui/blob/784fdc5/src/test/typescript/mockserver/metadata.ts#L246
  "datasetmetadata-from-ui-some.json with an additional DOI" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata-from-ui-some.json")
      .map(_.copy(identifiers = Some(Seq(SchemedValue(DatasetMetadata.doiScheme, "mocked-doi")))))
  )
  "datasetmetadata-from-ui-all.json without one of the authors" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata-from-ui-all.json"),
    expectedDdmContent =
      <ddm:profile>
        <dc:title xml:lang="nld">title 1</dc:title>
        <dc:title xml:lang="nld">title2</dc:title>
        <dcterms:description xml:lang="nld">description1</dcterms:description>
        <dcterms:description xml:lang="nld">description2</dcterms:description>
        <dcx-dai:creatorDetails>
          <dcx-dai:author>
            <dcx-dai:titles xml:lang="nld">Drs.</dcx-dai:titles>
            <dcx-dai:initials>D.A.</dcx-dai:initials>
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
        <dcterms:identifier xsi:type="id-type:DOI">doi:10.17632/DANS.6wg5xccnjd.1</dcterms:identifier>
        <dcterms:identifier xsi:type="id-type:ISBN">test identifier 1</dcterms:identifier>
        <dcterms:identifier xsi:type="id-type:NWO-PROJECTNR">test identifier 2</dcterms:identifier>
        <dcterms:identifier xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">archis nr. 1</dcterms:identifier>
        <dcterms:identifier xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">archis nr. 2</dcterms:identifier>
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
        <dcterms:type xsi:type="dcterms:DCMIType">Dataset</dcterms:type>
        <dcterms:type xsi:type="dcterms:DCMIType">Software</dcterms:type>
        <dcterms:type>drawings</dcterms:type>
        <dcterms:type>paintings</dcterms:type>
        <dcterms:format xsi:type="dcterms:IMT">text/plain</dcterms:format>
        <dcterms:format xsi:type="dcterms:IMT">image/tiff</dcterms:format>
        <dcterms:format>paperback</dcterms:format>
        <dcterms:format>audiobook</dcterms:format>
        <dcterms:format xsi:type="dcterms:IMT">application/x-cmdi+xml</dcterms:format>
        <dcterms:dateCopyrighted xsi:type="dcterms:W3CDTF">2018-03-18</dcterms:dateCopyrighted>
        <dcterms:valid xsi:type="dcterms:W3CDTF">2018-03-17</dcterms:valid>
        <dcterms:modified>2018-02-02</dcterms:modified>
        <dcterms:issued>Groundhog day</dcterms:issued>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
          <Point xmlns="http://www.opengis.net/gml">
            <pos>12 34</pos>
          </Point>
        </dcx-gml:spatial>
        <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
          <Point xmlns="http://www.opengis.net/gml">
            <pos>78 56</pos>
          </Point>
        </dcx-gml:spatial>
        <dcx-gml:spatial>
          <boundedBy xmlns="http://www.opengis.net/gml">
            <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
              <lowerCorner>4 3</lowerCorner>
              <upperCorner>2 1</upperCorner>
            </Envelope>
          </boundedBy>
        </dcx-gml:spatial>
        <dcx-gml:spatial>
          <boundedBy xmlns="http://www.opengis.net/gml">
            <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
              <lowerCorner>7 8</lowerCorner>
              <upperCorner>5 6</upperCorner>
            </Envelope>
          </boundedBy>
        </dcx-gml:spatial>
        <dcterms:license>http://creativecommons.org/publicdomain/zero/1.0</dcterms:license>
      </ddm:dcmiMetadata>
  )

  private def dcmiMetadata(actualDDM: Elem) = emptyDDM.copy(child =
    actualDDM \ "dcmiMetadata"
  )

  private def profileElements(actualDDM: Elem, label: String) = emptyDDM.copy(child =
    // prevent namespaces on children
    Elem("ddm", "profile", Null, emptyDDM.scope, minimizeEmpty = true, actualDDM \ "profile" \ label: _*)
  )

  private def parseTestResource(file: String) = Try {
    getManualTestResource(file)
  }.flatMap(DatasetMetadata(_))

}

trait DdmBehavior {
  this: TestSupportFixture =>

  val emptyDDM: Elem

  lazy val triedSchema: Try[Schema] = Try { // loading postponed until we actually start validating
    val schemas = emptyDDM.attribute("http://www.w3.org/2001/XMLSchema-instance", "schemaLocation")
      .getOrElse(fail("no schemaLocation attribute"))
      .headOption
      .getOrElse(fail("no schemaLocation value"))
      .text
      .replaceAll(".* ", "") :: Nil
    SchemaFactory
      .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      .newSchema(schemas.map(xsd => new StreamSource(xsd)).toArray[Source])
  }

  // pretty provides friendly trouble shooting for complex XML's
  private val prettyPrinter: PrettyPrinter = new scala.xml.PrettyPrinter(1024, 2)

  /**
   * @param input              to be converted to DDM
   * @param subset             subset of generated DDM that should match expectedOutput
   * @param expectedDdmContent members of <ddm:DDM> that should equal
   *                           the members of subset(DatsetXm(input.get))
   */
  def validDatasetMetadata(input: Try[DatasetMetadata],
                           subset: Elem => Elem = identity,
                           expectedDdmContent: Seq[Node] = Seq.empty
                          ): Unit = {
    lazy val datasetMetadata = input.getOrRecover(e => fail(e))
    lazy val triedDDM = DDM(datasetMetadata)

    if (expectedDdmContent.nonEmpty) it should "generate expected DDM" in {
      assumeSchemaAvailable
      prettyPrinter.format(subset(triedDDM.getOrRecover(e => fail(e)))) shouldBe
        prettyPrinter.format(emptyDDM.copy(child = expectedDdmContent))
    }

    it should "generate valid DDM" in {
      assumeSchemaAvailable
      triedDDM shouldBe a[Success[_]]
    }
  }
}
