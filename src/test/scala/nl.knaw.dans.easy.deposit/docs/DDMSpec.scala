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
import nl.knaw.dans.easy.deposit.Errors.InvalidDocumentException
import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.dm.DateScheme.W3CDTF
import nl.knaw.dans.easy.deposit.docs.dm._
import nl.knaw.dans.lib.error._
import org.scalatest.matchers.Matcher

import scala.util.{ Failure, Success, Try }
import scala.xml._

class DDMSpec extends TestSupportFixture with DdmBehavior {

  /** provides the verbose namespaces for inline DDM */
  lazy override val emptyDDM: Elem = DDM(new MinimalDatasetMetadata)
    .getOrRecover(fail(_))
    .copy(child = Seq())

  private val minimalDDM: NodeBuffer = <ddm:profile>
        <dc:title>Lorum ipsum</dc:title>
        <dcterms:description>dolor</dcterms:description>
        <dcx-dai:creatorDetails>
          <dcx-dai:author>
            <dcx-dai:initials>B.A.R.</dcx-dai:initials>
            <dcx-dai:surname>Foo</dcx-dai:surname>
          </dcx-dai:author>
        </dcx-dai:creatorDetails>
        <ddm:created>2018-05-29</ddm:created>
        <ddm:available>2018-07-30</ddm:available>
        <ddm:audience>D35200</ddm:audience>
        <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
      </ddm:profile>
      <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>

  "minimal" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata,
    expectedDdmContent = minimalDDM
  )

  private val emptyAuthor: Author = Author(role = Some(SchemedKeyValue("someScheme0", "someKey", "someValue")))
  private val emptyDate: Date = Date(Some(W3CDTF.toString), None, Some(DateQualifier.dateCopyrighted))
  private val minimalJson = new MinimalDatasetMetadata()
  private val mandatoryDates: Seq[Date] = minimalJson.datesCreated.toSeq ++ minimalJson.datesAvailable.toSeq
  // empty elements should be filtered when converting json to DDM
  "minimal with empty elements" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      dates = Some(mandatoryDates :+ emptyDate),
      types = Some(Seq(SchemedValue("someScheme1", ""))),
      formats = Some(Seq()),
      creators = minimalJson.creators.map(_ :+ emptyAuthor),
      contributors = Some(Seq()),
      subjects = Some(Seq(
        SchemedKeyValue("someScheme2", "someKey", ""),
        SchemedKeyValue("someScheme3", "", ""),
        SchemedKeyValue("dcterms:ISO3166", "", "someValue"),
      )),
      spatialCoverages = Some(Seq(
        SchemedKeyValue("someScheme4", "", ""),
        SchemedKeyValue("someScheme5", "someKey", ""),
        SchemedKeyValue("abr:ABRcomplex", "", "someValue"),
      )),
      temporalCoverages = Some(Seq()),
      relations = Some(Seq[RelationType](
        Relation(Some(RelationQualifier.isReferencedBy), Some(""), Some("")),
        Relation(Some(RelationQualifier.replaces), Some(""), None),
        Relation(Some(RelationQualifier.isRequiredBy), None, None),
        RelatedIdentifier(Some(""), Some(""), Some(RelationQualifier.isReplacedBy)),
        RelatedIdentifier(None, Some(""), Some(RelationQualifier.references)),
        RelatedIdentifier(None, None, Some(RelationQualifier.conformsTo)),
      )),
    ),
    expectedDdmContent = minimalDDM
  )

  "minimal with multiple titles" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(titles = Some(Seq("Lorum", "ipsum"))),
    subset = actualDDM => profileElements(actualDDM, "title"),
    expectedDdmContent =
      <ddm:profile>
        <dc:title>Lorum</dc:title>
        <dc:title>ipsum</dc:title>
      </ddm:profile>
  )

  "minimal with multiple descriptions" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(descriptions = Some(Seq(
      "Lorum <a href='mailto:does.not.exist@dans.knaw.nl'>ipsum</a>",
      "ipsum"
    ))),
    subset = actualDDM => profileElements(actualDDM, "description"),
    expectedDdmContent =
      <ddm:profile>
        <dcterms:description>Lorum &lt;a href='mailto:does.not.exist@dans.knaw.nl'&gt;ipsum&lt;/a&gt;</dcterms:description>
        <dcterms:description>ipsum</dcterms:description>
      </ddm:profile>
  )

  "minimal with multiple audiences" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(audiences = Some(Seq(
      SchemedKeyValue("blabla", "D11200", "Lorum"),
      SchemedKeyValue("blabla", "D32400", "ipsum")
    ))),
    subset = actualDDM => profileElements(actualDDM, "audience"),
    expectedDdmContent =
      <ddm:profile>
        <ddm:audience>D11200</ddm:audience>
        <ddm:audience>D32400</ddm:audience>
      </ddm:profile>
  )

  "minimal with multiple alternativeTitles" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(alternativeTitles = Some(Seq("Lorum", "ipsum"))),
    subset = actualDDM => dcmiMetadata(actualDDM),
    expectedDdmContent =
      <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcterms:alternative>Lorum</dcterms:alternative>
        <dcterms:alternative>ipsum</dcterms:alternative>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
  )

  "minimal with various author IDs" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(contributors = Some(Seq(
      Author(
        initials = Some("H."),
        surname = Some("Oefnix"),
        ids = Some(Seq(
          SchemedValue("DAI", "012345678x"),
          SchemedValue("id-type:ISNI", "000000012281955X"),
          SchemedValue("ORCID", "0000-0002-9079-593X"),
        ))
      ),
      Author(
        initials = Some("K.O."),
        surname = Some("Stunrix"),
        ids = Some(Seq(
          SchemedValue("id-type:DAI", "123456789x"),
          SchemedValue("id-type:ISNI", "ISNI:000000012281955X"),
        ))
      ),
    ))),
    subset = actualDDM => dcmiMetadata(actualDDM),
    expectedDdmContent =
      <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcx-dai:contributorDetails>
          <dcx-dai:author>
            <dcx-dai:initials>H.</dcx-dai:initials>
            <dcx-dai:surname>Oefnix</dcx-dai:surname>
            <dcx-dai:DAI>012345678x</dcx-dai:DAI>
            <dcx-dai:ISNI>000000012281955X</dcx-dai:ISNI>
            <dcx-dai:ORCID>0000-0002-9079-593X</dcx-dai:ORCID>
          </dcx-dai:author>
        </dcx-dai:contributorDetails>
        <dcx-dai:contributorDetails>
          <dcx-dai:author>
            <dcx-dai:initials>K.O.</dcx-dai:initials>
            <dcx-dai:surname>Stunrix</dcx-dai:surname>
            <dcx-dai:DAI>123456789x</dcx-dai:DAI>
            <dcx-dai:ISNI>ISNI:000000012281955X</dcx-dai:ISNI>
          </dcx-dai:author>
        </dcx-dai:contributorDetails>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
  )

  "invalid name space for author ID" should "fail" in {
    val author = Author(
      initials = Some("F.O.O."),
      surname = Some("Bar"),
      ids = Some(Seq(SchemedValue("dcx-dai:ISNI", "ISNI:000000012281955X")))
    )
    DDM(new MinimalDatasetMetadata(contributors = Some(Seq(author)))) should
      beInvalidDoc("invalid DatasetMetadata: expecting (label) or (prefix:label); got [dcx-dai:dcx-dai:ISNI] to adjust the <label> of <label>ISNI:000000012281955X</label>")
  }

  "date without qualifier" should "fail" in {
    DDM(new MinimalDatasetMetadata(
      dates = Some(mandatoryDates :+ Date(None, Some("2020"), None)),
    )) should beInvalidDoc("""invalid DatasetMetadata: no qualifier for Date: {"value":"2020"}""")
  }

  "relatedIdentifier without qualifier" should "fail" in {
    val relatedIdentifier = RelatedIdentifier(scheme = Some("x"), value = Some("rabarbera"), qualifier = None)
    DDM(new MinimalDatasetMetadata(relations = Some(Seq[RelationType](relatedIdentifier)))) should
      beInvalidDoc("""invalid DatasetMetadata: no qualifier for RelatedIdentifier: {"scheme":"x","value":"rabarbera"}""")
  }

  "relation without qualifier" should "fail" in {
    val url = "https://does.no.exist.dans.knaw.nl"
    val relation = Relation(qualifier = None, Some(url), title = Some("blabla"))
    DDM(new MinimalDatasetMetadata(relations = Some(Seq[RelationType](relation)))) should
      beInvalidDoc(s"""invalid DatasetMetadata: no qualifier for Relation: {"url":"$url","title":"blabla"}""")
  }

  "spatial box without west" should "fail" in {
    val box = """{"scheme":"x","north":"1","east":"2","south":"3"}"""
    DDM(new MinimalDatasetMetadata(
      spatialBoxes = parse(s"""{"spatialBoxes":[$box]}""").spatialBoxes
    )) should beInvalidDoc(s"""invalid DatasetMetadata: not all of (north,east,south,west) provided by SpatialBox: $box""")
  }

  "spatial box without scheme" should "fail" in {
    // note the differences in white spaces and quotes for the values
    DDM(new MinimalDatasetMetadata(
      spatialBoxes = parse("""{"spatialBoxes":[{"north": 1, "east": 2, "south": 3, "west": 4}]}""").spatialBoxes
    )) should beInvalidDoc("""invalid DatasetMetadata: scheme is mandatory for SpatialBox: {"north":"1","east":"2","south":"3","west":"4"}""")
  }

  "spatial point without scheme" should "fail" in {
    val point = """{"x":"1","y":"2"}"""
    DDM(new MinimalDatasetMetadata(
      spatialPoints = parse(s"""{"spatialPoints":[$point]}""").spatialPoints
    )) should beInvalidDoc(s"""invalid DatasetMetadata: scheme is mandatory for SpatialPoint: $point""")
  }

  "language key without scheme" should "fail" in {
    DDM(new MinimalDatasetMetadata(
      languagesOfFiles = parse("""{"languagesOfFiles":[ {"key":"NL","value":""}]}""").languagesOfFiles,
    )) should beInvalidDoc(s"""invalid DatasetMetadata: SchemedKeyValue needs a scheme for a key {"key":"NL","value":""}""")
  }

  "language key with empty scheme" should "fail" in {
    DDM(new MinimalDatasetMetadata(
      languagesOfFiles = parse("""{"languagesOfFiles":[ {"scheme":" " "key":"NL","value":""}]}""").languagesOfFiles,
    )) should beInvalidDoc(s"""invalid DatasetMetadata: SchemedKeyValue needs a scheme for a key {"scheme":" ","key":"NL","value":""}""")
  }

  "multiple datesCreated" should "fail" in {
    the[IllegalArgumentException] thrownBy new DatasetMetadata(
      dates = Some(mandatoryDates ++ mandatoryDates),
    ) should have message
      """requirement failed: At most one allowed; got [{"scheme":"dcterms:W3CDTF","value":"2018-05-29","qualifier":"dcterms:created"},{"scheme":"dcterms:W3CDTF","value":"2018-05-29","qualifier":"dcterms:created"}]"""
  }

  "metadata without instructionsForReuse" should behave like validDatasetMetadata(
    // the only element of <ddm:profile> that is not mandatory
    input = new MinimalDatasetMetadata(instructionsForReuse = None)
  )

  "metadata without titles" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(titles = None)
  )

  "minimal without descriptions" should behave like validDatasetMetadata(
      input = new MinimalDatasetMetadata(descriptions = None)
  )

  "metadata without audiences" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(audiences = None)
  )

  "metadata without creators" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(creators = None)
  )

  "metadata without dateCreated" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(dates = Some(Seq(mandatoryDates.head)))
  )

  "metadata without dateAvailable" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(dates = Some(Seq(mandatoryDates.last)))
  )

  "metadata without accessRights" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(accessRights = None)
  )

  "minimal with SchemedKeyValue variants" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      subjects = parse("""{"subjects":[ {"key":"","value":"Overflakees"}, {"key":"FR","value":""}, {"key":"EN"} ]}""").subjects,
      languagesOfFiles = parse("""{"languagesOfFiles":[ {"key":" ","value":"Goerees"}, {"value":"Frysk"}, {"key":" ","value":""}]}""").languagesOfFiles,
    ),
    subset = actualDDM => dcmiMetadata(actualDDM),
    expectedDdmContent =
      <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dc:subject>Overflakees</dc:subject>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        <dcterms:language>Goerees</dcterms:language>
        <dcterms:language>Frysk</dcterms:language>
      </ddm:dcmiMetadata>
  )

  "minimal with rightsHolders" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      creators = parse(
        """{"creators":[
          |  {"initials":"F.O.O.","surname":"Bar"},
          |  {"initials":"O.","surname":"Belix","role":{"scheme":"blabla","key":"RightsHolder","value":"Lorum Ipsum"}}
          |]}"""
      ).creators,
      contributors = parse(
        """{"contributors":[
          |  {"initials":"A.S.","surname":"Terix","role":{"scheme":"blabla","key":"RightsHolder","value":"Lorum Ipsum"}}
          |]}""").contributors,
    ),
    subset = actualDDM => dcmiMetadata(actualDDM),
    expectedDdmContent =
      //N.B: creators in ddm:profile unless they are rightsHolders
      // To preserve ID's a copy of the rights holders appears under creators and/or contributors
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcx-dai:contributorDetails>
            <dcx-dai:author>
              <dcx-dai:initials>A.S.</dcx-dai:initials>
              <dcx-dai:surname>Terix</dcx-dai:surname>
              <dcx-dai:role>RightsHolder</dcx-dai:role>
            </dcx-dai:author>
          </dcx-dai:contributorDetails>
          <dcterms:rightsHolder>A.S. Terix</dcterms:rightsHolder>
          <dcterms:rightsHolder>O. Belix</dcterms:rightsHolder>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        </ddm:dcmiMetadata>
  )

  "minimal with all types of dates (except submitted)" should behave like {
    val date = "2018-06-14"
    val dates = DateQualifier.values.toSeq
      .withFilter(_ != DateQualifier.dateSubmitted)
      .map { qualifier => Date(Some(W3CDTF.toString), Some(date), Some(qualifier)) }
    validDatasetMetadata(
      input = new MinimalDatasetMetadata(dates = Some(dates)),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent =
        // the dateSubmitted specified above is replaced by "now" as set by the fixture
        // dateCreated and dateAvailable are documented with the pure minimal test
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
          <dcterms:date xsi:type="dcterms:W3CDTF">{ date }</dcterms:date>
          <dcterms:dateAccepted xsi:type="dcterms:W3CDTF">{ date }</dcterms:dateAccepted>
          <dcterms:dateCopyrighted xsi:type="dcterms:W3CDTF">{ date }</dcterms:dateCopyrighted>
          <dcterms:issued xsi:type="dcterms:W3CDTF">{ date }</dcterms:issued>
          <dcterms:modified xsi:type="dcterms:W3CDTF">{ date }</dcterms:modified>
          <dcterms:valid xsi:type="dcterms:W3CDTF">{ date }</dcterms:valid>
        </ddm:dcmiMetadata>
    )
  }

  "minimal with various types of dates" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(dates = Some(Seq(
      Date(scheme = None, value = Some("2018"), Some(DateQualifier.created)),
      Date(scheme = None, value = Some("2018"), Some(DateQualifier.available)),
      Date(scheme = None, value = Some("Groundhog day"), Some(DateQualifier.dateAccepted)),
      Date(scheme = None, value = Some("Groundhog day"), Some(DateQualifier.dateCopyrighted)),
      Date(scheme = None, value = Some("Groundhog day"), Some(DateQualifier.issued)),
      Date(scheme = None, value = Some("Groundhog day"), Some(DateQualifier.modified)),
      Date(scheme = None, value = Some("Groundhog day"), Some(DateQualifier.valid)),
      Date(scheme = Some(W3CDTF.toString), value = Some("2018"), Some(DateQualifier.valid)),
      Date(scheme = Some(W3CDTF.toString), value = Some("2018-12"), Some(DateQualifier.valid)),
      Date(scheme = Some(W3CDTF.toString), value = Some("2018-12-09T08:15:30-05:00"), Some(DateQualifier.valid)),
      Date(scheme = Some(W3CDTF.toString), value = Some("2018-12-09T13:15:30Z"), Some(DateQualifier.valid)),
    ))),
    subset = actualDDM => dcmiMetadata(actualDDM),
    expectedDdmContent =
        <ddm:dcmiMetadata>
          <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
          <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">{ nowYMD }</dcterms:dateSubmitted>
          <dcterms:dateAccepted>Groundhog day</dcterms:dateAccepted>
          <dcterms:dateCopyrighted>Groundhog day</dcterms:dateCopyrighted>
          <dcterms:issued>Groundhog day</dcterms:issued>
          <dcterms:modified>Groundhog day</dcterms:modified>
          <dcterms:valid>Groundhog day</dcterms:valid>
          <dcterms:valid xsi:type="dcterms:W3CDTF">2018</dcterms:valid>
          <dcterms:valid xsi:type="dcterms:W3CDTF">2018-12</dcterms:valid>
          <dcterms:valid xsi:type="dcterms:W3CDTF">2018-12-09</dcterms:valid>
          <dcterms:valid xsi:type="dcterms:W3CDTF">2018-12-09</dcterms:valid>
        </ddm:dcmiMetadata>
  )

  "minimal with points and boxes of issue 1538" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      spatialPoints = parse(
        """{"spatialPoints": [
          |  { "scheme": "RD", "x": 79500, "y": 446750 },
          |  { "scheme": "degrees", "x": 52.0811, "y": 4.34521 }
          |]}""").spatialPoints,
      spatialBoxes = parse(
        """{"spatialBoxes": [{
          |  "scheme": "RD",
          |  "north": 486890.5,
          |  "east": 121811.88,
          |  "south": 436172.5,
          |  "west": 91232.016
          |  }
          |]}""").spatialBoxes,
    ),
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

  "minimal with points from split-multi-deposit" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      spatialPoints = parse(
        """{  "spatialPoints":[
          |  { "x": 1, "y": 2, "scheme":"RD" },
          |  { "x": 3, "y": 4, "scheme":"http://some.example.com" },
          |  { "x": 5, "y": 6, "scheme":"degrees" },
          |]}""").spatialPoints,
    ),
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

  "minimal with boxes from split-multi-deposit" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      spatialBoxes = parse(
        """{  "spatialBoxes":[
          |  {"north": 1, "south": 2,  "east": 3,  "west": 4, "scheme":"RD"},
          |  {"north": 5, "south": 6,  "east": 7,  "west": 8, "scheme":"xyz"},
          |  {"north": 9, "south": 10, "east": 11, "west": 12, "scheme":"degrees"}
          |]}"""
      ).spatialBoxes),
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

  "issue-1538.json" should behave like validDatasetMetadata(
    input = parseTestResource("issue-1538.json").setDoi("mocked_DOI")
  )

  "datasetmetadata.json" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata.json").setDoi("mocked_DOI")
  )
  // TODO keep resources in sync with UI module: https://github.com/DANS-KNAW/easy-deposit-ui/blob/784fdc5/src/test/typescript/mockserver/metadata.ts#L246
  "datasetmetadata-from-ui-some.json with an additional DOI" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata-from-ui-some.json")
      .copy(identifiers = Some(Seq(SchemedValue(DatasetMetadata.doiScheme, "mocked-doi"))))
  )
  "datasetmetadata-from-ui-all.json without one of the authors" should behave like validDatasetMetadata(
    input = parseTestResource("datasetmetadata-from-ui-all.json"),
    expectedDdmContent =
      <ddm:profile>
        <dc:title xml:lang="nld">title 1</dc:title>
        <dc:title xml:lang="nld">title2</dc:title>
        <dcterms:description xml:lang="nld">description1</dcterms:description>
        <dcterms:description xml:lang="nld">description2</dcterms:description>
        <ddm:description descriptionType="TechnicalInfo">remark1</ddm:description>
        <ddm:description descriptionType="TechnicalInfo">remark2</ddm:description>
        <dcx-dai:creatorDetails>
          <dcx-dai:author>
            <dcx-dai:titles xml:lang="nld">Drs.</dcx-dai:titles>
            <dcx-dai:initials>D.A.</dcx-dai:initials>
            <dcx-dai:surname>NS</dcx-dai:surname>
            <dcx-dai:DAI>93313935x</dcx-dai:DAI>
            <dcx-dai:ORCID>0000-0002-9079-593X</dcx-dai:ORCID>
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
        <dcterms:identifier xsi:type="id-type:DOI">10.17632/DANS.6wg5xccnjd.1</dcterms:identifier>
        <dcterms:alternative xml:lang="nld">alternative title 1</dcterms:alternative>
        <dcterms:alternative xml:lang="nld">alternative title2</dcterms:alternative>
        <dcterms:isReferencedBy xsi:type="id-type:ISSN">2123-34X</dcterms:isReferencedBy>
        <ddm:conformsTo xml:lang="nld" href="http://x">title1</ddm:conformsTo>
        <dcterms:requires xml:lang="nld">title2</dcterms:requires>
        <ddm:isReplacedBy href="http://y">http://y</ddm:isReplacedBy>
        <ddm:relation href="http://z">http://z</ddm:relation>
        <dcterms:relation xml:lang="nld">title3</dcterms:relation>
        <dcterms:isFormatOf xsi:type="id-type:ISBN">test identifier 1</dcterms:isFormatOf>
        <dcterms:isFormatOf xsi:type="id-type:NWO-PROJECTNR">test identifier 2</dcterms:isFormatOf>
        <dcterms:isFormatOf xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">archis nr. 1</dcterms:isFormatOf>
        <dcterms:isFormatOf xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">archis nr. 2</dcterms:isFormatOf>
        <ddm:isFormatOf scheme="id-type:DOI" href="https://doi.org/10.5072/test-doi">10.5072/test-doi</ddm:isFormatOf>
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
        <dcx-dai:contributorDetails>
          <dcx-dai:organization>
            <dcx-dai:name xml:lang="nld">rightsHolder1</dcx-dai:name>
            <dcx-dai:role>RightsHolder</dcx-dai:role>
          </dcx-dai:organization>
        </dcx-dai:contributorDetails>
        <dcx-dai:contributorDetails>
          <dcx-dai:author>
            <dcx-dai:titles xml:lang="nld">Dr.</dcx-dai:titles>
            <dcx-dai:initials>A.S.</dcx-dai:initials>
            <dcx-dai:insertions>van</dcx-dai:insertions>
            <dcx-dai:surname>Terix</dcx-dai:surname>
            <dcx-dai:role>RightsHolder</dcx-dai:role>
          </dcx-dai:author>
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
        <dc:subject xml:lang="nld">subject1</dc:subject>
        <dc:subject xml:lang="nld">subject2</dc:subject>
        <dcterms:subject xsi:type="abr:ABRcomplex">RKER</dcterms:subject>
        <dcterms:subject xsi:type="abr:ABRcomplex">VK</dcterms:subject>
        <dcterms:temporal xsi:type="abr:ABRperiode">ROMLA</dcterms:temporal>
        <dcterms:temporal xsi:type="abr:ABRperiode">ROMLB</dcterms:temporal>
        <dcterms:temporal xml:lang="nld">temp1</dcterms:temporal>
        <dcterms:temporal xml:lang="nld">temp2</dcterms:temporal>
        <dcterms:spatial xsi:type="dcterms:ISO3166">NLD</dcterms:spatial>
        <dcterms:spatial xml:lang="nld">Haringvliet</dcterms:spatial>
        <dcterms:spatial xml:lang="nld">Grevelingenmeer</dcterms:spatial>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        <dcterms:dateCopyrighted xsi:type="dcterms:W3CDTF">2018-03-18</dcterms:dateCopyrighted>
        <dcterms:valid xsi:type="dcterms:W3CDTF">2018-03-17</dcterms:valid>
        <dcterms:modified>2018-02-02</dcterms:modified>
        <dcterms:issued>Groundhog day</dcterms:issued>
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
        <dcterms:license xsi:type="dcterms:URI">http://creativecommons.org/publicdomain/zero/1.0</dcterms:license>
        <dcterms:language xsi:type="dcterms:ISO639-2">eng</dcterms:language>
        <dcterms:language xsi:type="dcterms:ISO639-2">nld</dcterms:language>
        <dcterms:language>Flakkees</dcterms:language>
        <dcterms:language>Goerees</dcterms:language>
      </ddm:dcmiMetadata>
  )

  private def dcmiMetadata(actualDDM: Elem) = emptyDDM.copy(child =
    actualDDM \ "dcmiMetadata"
  )

  private def profileElements(actualDDM: Elem, label: String) = emptyDDM.copy(child =
    // prevent namespaces on children
    Elem("ddm", "profile", Null, emptyDDM.scope, minimizeEmpty = true, actualDDM \ "profile" \ label: _*)
  )

  private def beInvalidDoc(msg: String): Matcher[Any] = {
    matchPattern {
      case Failure(e: InvalidDocumentException) if e.getMessage ==
        msg =>
    }
  }

  private def parse(input: String) = {
    DatasetMetadata(input.stripMargin)
      .getOrRecover(e => fail(s"could not read test input $input", e))
  }

  private def parseTestResource(file: String) = {
    Try(getManualTestResource(file))
      .flatMap(DatasetMetadata(_))
      .getOrRecover(e => fail(s"could not parse test inpyt $file", e))
  }
}

trait DdmBehavior {
  this: TestSupportFixture =>

  val emptyDDM: Elem
  lazy val triedSchema: Try[Schema] = DDM.loadSchema

  /**
   * @param input              to be converted to DDM
   * @param subset             subset of generated DDM that should match expectedOutput
   * @param expectedDdmContent members of <ddm:DDM> that should equal
   *                           the members of subset(DatsetXm(input.get))
   */
  def validDatasetMetadata(input: DatasetMetadata,
                           subset: Elem => Elem = identity,
                           expectedDdmContent: Seq[Node] = Seq.empty
                          ): Unit = {
    lazy val triedDDM = DDM(input)

    if (expectedDdmContent.nonEmpty) it should "generate expected DDM" in {
      prettyPrinter.format(subset(triedDDM.getOrRecover(e => fail(e)))) shouldBe
        prettyPrinter.format(emptyDDM.copy(child = expectedDdmContent))
    }

    it should "generate valid DDM" in {
      triedDDM shouldBe a[Success[_]]
      val ddm = triedDDM.getOrRecover(e => fail("should not get past the check above and fail here", e))

      assume(triedSchema.isAvailable)
      val result = triedSchema.validate(ddm)
      if (result.isFailure) // show the relevant XML section to troubleshoot a broken test
        println(prettyPrinter.format(subset(triedDDM.getOrRecover(e => fail(e)))))
      result shouldBe a[Success[_]]
    }
  }
}
