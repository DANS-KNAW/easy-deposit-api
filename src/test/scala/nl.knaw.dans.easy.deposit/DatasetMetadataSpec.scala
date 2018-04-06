package nl.knaw.dans.easy.deposit

import nl.knaw.dans.easy.deposit.components.Json

class DatasetMetadataSpec extends TestSupportFixture {
  private val example =
    """{
      |  "doi": "doi:10.17632/DANS.6wg5xccnjd.1",
      |  "urn": "string",
      |  "titles": [
      |    "Title 1",
      |    "Title 2"
      |  ],
      |  "alternativeTitles": [
      |    "string"
      |  ],
      |  "creators": [
      |    {
      |      "titles": "Prof Dr",
      |      "initials": "A",
      |      "insertions": null,
      |      "surname": "Einstein"
      |    }
      |  ],
      |  "contributors": [
      |    {
      |      "titles": "Prof Dr",
      |      "initials": "A",
      |      "insertions": null,
      |      "surname": "Einstein"
      |    }
      |  ],
      |  "created": "string",
      |  "descriptions": [
      |    "Description 1"
      |  ],
      |  "audiences": [
      |    "string"
      |  ],
      |  "archisNrs": [
      |    "string"
      |  ],
      |  "subjectsAbrComplex": [
      |    "string"
      |  ],
      |  "subjects": [
      |    "string"
      |  ],
      |  "temporalCoveragesAbr": [
      |    "string"
      |  ],
      |  "temporalCoverages": [
      |    "string"
      |  ],
      |  "spatialPoints": [
      |    {
      |      "scheme": "string",
      |      "x": "string",
      |      "y": "string"
      |    }
      |  ],
      |  "spatialBoxes": [
      |    {
      |      "scheme": "string",
      |      "north": "string",
      |      "east": "string",
      |      "south": "string",
      |      "west": "string"
      |    }
      |  ],
      |  "spatialCoverages": [
      |    "string"
      |  ],
      |  "identifiers": [
      |    {
      |      "scheme": "string",
      |      "identifier": "string"
      |    }
      |  ],
      |  "relations": [
      |    {
      |      "qualifier": "string",
      |      "url": "string",
      |      "title": "string"
      |    }
      |  ],
      |  "typesDcmi": [
      |    "string"
      |  ],
      |  "types": [
      |    "string"
      |  ],
      |  "formatsMediaType": [
      |    "string"
      |  ],
      |  "formats": [
      |    "string"
      |  ],
      |  "languagesIso639": [
      |    "string"
      |  ],
      |  "languages": [
      |    "string"
      |  ],
      |  "sources": [
      |    "string"
      |  ],
      |  "datesIso8601": [
      |    {
      |      "scheme": "string",
      |      "date": "string"
      |    }
      |  ],
      |  "dates": [
      |    {
      |      "scheme": "string",
      |      "date": "string"
      |    }
      |  ],
      |  "remarks": "string",
      |  "rightsHolders": [
      |    "string"
      |  ],
      |  "publishers": [
      |    "string"
      |  ],
      |  "dateAvailable": "string"
      |}""".stripMargin

  "deserialization and serialisation" should "at most have different white space and order of fields may vary" in {
    Json.toJson(Json.getDatasetMetadata(example).getOrElse("")) shouldBe example.split("\n").map(_.trim.replaceAll(": ",":")).mkString

  }
  "serialization and deserialisation" should "return the same object" ignore {

  }
}
