package nl.knaw.dans.easy.deposit

import nl.knaw.dans.easy.deposit.components.Json
import nl.knaw.dans.easy.deposit.components.Json.InvalidDocument

import scala.util.{ Failure, Success }

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

  "deserialization/serialisation" should "at most have different white space and different order of fields" in {
    val expected = example.split("\n").map(_.trim.replaceAll(": ", ":")).mkString
    // JObject(List((doi,JString(doi:10.17632/DANS.6wg5xccnjd.1)), ...
    val result = Json.toJson(Json.getDatasetMetadata(example).getOrElse(""))
    result.length shouldBe expected.length
    //result shouldBe expected // won't work because of random order of class members
  }

  "deserialization" should "ignore additional info" in {
    //JObject(List((x,JInt(1))))
    inside(Json.getDatasetMetadata("""{"x":1}""")) {
      case Success(_: DatasetMetadata) =>
    }
    inside(Json.toDatasetMetadata("""{"x":1}""")) {
      case Success(_: DatasetMetadata) =>
    }
  }

  it should "be happy with empty objects" in {
    // JObject(List())
    inside(Json.getDatasetMetadata("""{}{}""")) { case Success(_: DatasetMetadata) => }
    inside(Json.toDatasetMetadata("""{}{}""")) { case Success(_: DatasetMetadata) => }
  }

  it should "fail on an empty array" in {
    // JArray(List())
    inside(Json.getDatasetMetadata("""[]""")) { case Success(_: DatasetMetadata) => }
    inside(Json.toDatasetMetadata("""[]""")) { case Success(_: DatasetMetadata) => }
  }

  it should "fail on empty arrays" in {
    // JArray(List())
    inside(Json.getDatasetMetadata("""[][]""")) { case Success(_: DatasetMetadata) => }
    inside(Json.toDatasetMetadata("""[][]""")) { case Success(_: DatasetMetadata) => }
  }

  it should "not accept a literal number" in {
    inside(Json.getDatasetMetadata("""123""")) { case Failure(_: InvalidDocument) => }
    inside(Json.toDatasetMetadata("""123""")) { case Failure(_: InvalidDocument) => }
  }
}
