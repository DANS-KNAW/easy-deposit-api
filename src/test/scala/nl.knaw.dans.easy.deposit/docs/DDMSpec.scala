/*
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
import nl.knaw.dans.easy.deposit.docs.dm.DateScheme.W3CDTF
import nl.knaw.dans.easy.deposit.docs.dm._
import nl.knaw.dans.lib.error._

import javax.xml.validation.Schema
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

  "descriptions with complex white space" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      descriptions = Some(Seq(
        "first",
        """par1
          |par  2
          |
          |par4""".stripMargin
      )),
      instructionsForReuse = Some(Seq(
        """blabla
          |rabarbera""".stripMargin
      )),
    ),
    subset = actualDDM => profileElements(actualDDM, "description"),
    expectedDdmContent =
      <ddm:profile>
        <dcterms:description>first</dcterms:description>
        <dcterms:description>par1
          par  2
          par4</dcterms:description>
        <ddm:description descriptionType="TechnicalInfo">blabla rabarbera</ddm:description>
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

  "date with default qualifier" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(dates = Some(mandatoryDates :+ Date(None, Some("2020"), None))),
    subset = actualDDM => dcmiMetadata(actualDDM),
    expectedDdmContent = <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        <dcterms:date>2020</dcterms:date>
      </ddm:dcmiMetadata>
  )

  "relatedIdentifier with default qualifier" should behave like {
    val relatedIdentifier = RelatedIdentifier(scheme = Some("id-type:ISSN"), value = Some("rabarbera"), qualifier = None)
    validDatasetMetadata(
      input = new MinimalDatasetMetadata(relations = Some(Seq[RelationType](relatedIdentifier))),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent = <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcterms:relation xsi:type="id-type:ISSN">rabarbera</dcterms:relation>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
    )
  }

  "relation with default qualifier" should behave like {
    val relation = Relation(qualifier = None, Some("https://does.no.exist.dans.knaw.nl"), title = Some("blabla"))
    validDatasetMetadata(
      input = new MinimalDatasetMetadata(relations = Some(Seq[RelationType](relation))),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent = <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <ddm:relation href="https://does.no.exist.dans.knaw.nl">blabla</ddm:relation>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
      </ddm:dcmiMetadata>
    )
  }

  "spatials with zero defaults" should behave like {
    val boxes = """{"scheme":"RD","north":"1","east":"2","south":"3"},{"scheme":"degrees","east":"4"},{"scheme":"z"}"""
    val points = """{"x":"5","y":"6"},{"x":"7"},{"y":"8"},{"scheme":"9"}"""
    validDatasetMetadata(
      input = new MinimalDatasetMetadata(
        spatialBoxes = parse(s"""{"spatialBoxes":[$boxes]}""").spatialBoxes,
        spatialPoints = parse(s"""{"spatialPoints":[$points]}""").spatialPoints,
      ),
      subset = actualDDM => dcmiMetadata(actualDDM),
      expectedDdmContent = <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="id-type:DOI">mocked-DOI</dcterms:identifier>
        <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">2018-03-22</dcterms:dateSubmitted>
        <dcx-gml:spatial>
          <Point xmlns="http://www.opengis.net/gml">
            <pos>6 5</pos>
          </Point>
        </dcx-gml:spatial>
        <dcx-gml:spatial>
          <Point xmlns="http://www.opengis.net/gml">
            <pos>0 7</pos>
          </Point>
        </dcx-gml:spatial>
        <dcx-gml:spatial>
          <Point xmlns="http://www.opengis.net/gml">
            <pos>8 0</pos>
          </Point>
        </dcx-gml:spatial>
        <dcx-gml:spatial>
          <boundedBy xmlns="http://www.opengis.net/gml">
            <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
              <lowerCorner>0 3</lowerCorner>
              <upperCorner>2 1</upperCorner>
            </Envelope>
          </boundedBy>
        </dcx-gml:spatial>
        <dcx-gml:spatial>
          <boundedBy xmlns="http://www.opengis.net/gml">
            <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
              <lowerCorner>0 0</lowerCorner>
              <upperCorner>0 4</upperCorner>
            </Envelope>
          </boundedBy>
        </dcx-gml:spatial>
      </ddm:dcmiMetadata>
    )
  }

  "language key without scheme" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      languagesOfFiles = parse("""{"languagesOfFiles":[ {"key":"NL","value":""}]}""").languagesOfFiles,
    )
  )

  "language key with empty scheme" should behave like validDatasetMetadata(
    input = new MinimalDatasetMetadata(
      languagesOfFiles = parse("""{"languagesOfFiles":[ {"scheme":" " "key":"NL","value":""}]}""").languagesOfFiles,
    )
  )

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

  // documenting (not exhaustive) what the client should validate to get the metadata ingested
  "Schema.validate(DDM(json))" should "report missing titles" in {
    val metadata = new MinimalDatasetMetadata(titles = None)
    assume(triedSchema.isAvailable)
    val result = validate(metadata)
    result should
      matchSaxMessage(".*Invalid content was found starting with element '.*:description.'. One of '.*:title.' is expected.")
  }

  it should "report missing descriptions" in {
    val metadata = new MinimalDatasetMetadata(descriptions = None)
    assume(triedSchema.isAvailable)
    validate(metadata) should
      matchSaxMessage(".*Invalid content was found starting with element '.*:creatorDetails.'. One of '.*:description.' is expected.")
  }

  it should "report missing audiences" in {
    val metadata = new MinimalDatasetMetadata(audiences = None)
    assume(triedSchema.isAvailable)
    validate(metadata) should
      matchSaxMessage(".*Invalid content was found starting with element '.*:accessRights.'. One of '.*:audience.' is expected.")
  }

  it should "report missing creators" in {
    val metadata = new MinimalDatasetMetadata(creators = None)
    assume(triedSchema.isAvailable)
    validate(metadata) should
      matchSaxMessage(".*Invalid content was found starting with element '.*:created.'. One of '.*:description, .*:creator.' is expected.")
  }

  it should "report missing dateCreated" in {
    val metadata = new MinimalDatasetMetadata(dates = Some(Seq(mandatoryDates.head)))
    assume(triedSchema.isAvailable)
    validate(metadata) should
      matchSaxMessage(".*Invalid content was found starting with element '.*:audience.'. One of '.*:available.' is expected.")
  }

  it should "report missing dateAvailable" in {
    val metadata = new MinimalDatasetMetadata(dates = Some(Seq(mandatoryDates.last)))
    assume(triedSchema.isAvailable)
    validate(metadata) should
      matchSaxMessage(".*Invalid content was found starting with element '.*:available.'. One of '.*:creator, .*:created.' is expected.")
  }

  it should "report missing accessRights" in {
    val metadata = new MinimalDatasetMetadata(accessRights = None)
    assume(triedSchema.isAvailable)
    validate(metadata) should
      matchSaxMessage(".*The content of element 'ddm:profile' is not complete. One of '.*:audience, .*:accessRights.' is expected.")
  }

  it should "report spatial box without scheme" in {
    val box = """{"north": 1, "east": 2, "south": 3, "west": 4}"""
    val metadata = new MinimalDatasetMetadata(
      spatialBoxes = parse(s"""{"spatialBoxes":[$box]}""").spatialBoxes
    )
    assume(triedSchema.isAvailable)
    validate(metadata) should matchSaxMessage(".*Attribute 'srsName' must appear on element 'Envelope'.")
  }

  it should "report an invalid name space for author ID" in {
    val author =
      """{
        |  "initials":"F.O.O.",
        |  "surname":"Bar",
        |  "ids":[{"scheme":"dcx-dai:ISNI","value":"ISNI:000000012281955X"}]
        |}"""
    val ddm = toDDM(new MinimalDatasetMetadata(contributors = parse(s"""{"contributors":[$author]}""").contributors))
    prettyPrinter.format(ddm) should include("<dcx-dai:dcx-dai:ISNI>ISNI:000000012281955X</dcx-dai:dcx-dai:ISNI>")
    assume(triedSchema.isAvailable)
    triedSchema.validate(ddm) should matchSaxMessage(".*dcx-dai:dcx-dai.*") // note the duplication
  }

  it should "report an invalid id-type for author" in {
    val author =
      """{
        |  "initials":"F.O.O.",
        |  "surname":"Bar",
        |  "ids":[{"scheme":"id-type:foo","value":"bar"}]
        |}"""
    val ddm = toDDM(new MinimalDatasetMetadata(contributors = parse(s"""{"contributors":[$author]}""").contributors))
    prettyPrinter.format(ddm) should include("<dcx-dai:foo>bar</dcx-dai:foo>")
    assume(triedSchema.isAvailable)
    triedSchema.validate(ddm) should matchSaxMessage(".*:foo.*")
  }

  "minimal with SchemedKeyValue variants" should behave like {
    val subjects = """{"key":"","value":"Overflakees"}, {"key":"FR","value":""}, {"key":"EN"}"""
    val languages = """{"key":" ","value":"Goerees"}, {"value":"Frysk"}, {"key":" ","value":""}"""
    validDatasetMetadata(
      input = new MinimalDatasetMetadata(
        subjects = parse(s"""{"subjects":[$subjects ]}""").subjects,
        languagesOfFiles = parse(s"""{"languagesOfFiles":[$languages]}""").languagesOfFiles,
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
  }

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

  private def dcmiMetadata(actualDDM: Elem) = emptyDDM.copy(child =
    actualDDM \ "dcmiMetadata"
  )

  private def profileElements(actualDDM: Elem, label: String) = emptyDDM.copy(child =
    // prevent namespaces on children
    Elem("ddm", "profile", Null, emptyDDM.scope, minimizeEmpty = true, actualDDM \ "profile" \ label: _*)
  )

  private def matchSaxMessage(regexp: String) = {
    matchPattern { case Failure(e: SAXParseException) if e.getMessage.matches(regexp) => }
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

  private def validate(metadata: MinimalDatasetMetadata) = {
    triedSchema.validate(toDDM(metadata))
  }

  private def toDDM(metadata: MinimalDatasetMetadata) = {
    DDM(metadata).getOrRecover(e => fail("could not create test data", e))
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
