package nl.knaw.dans.easy.deposit

import better.files.File

class MaintenanceSpec extends TestSupportFixture {

  "test classes" should "test all zips" in {
    File("src/test/scala").walk()
      .filter(_.name.endsWith("Spec.scala"))
      .toArray
      .flatMap(_.contentAsString.split("[\r\n]+").filter(_.contains("/manual-test/")))
      .map(_.replaceAll(".*/manual-test/", "").replaceAll("\".*", ""))
      .filter(str => !str.isEmpty && str.endsWith(".zip"))
      .distinct should
      contain allElementsOf File("src/test/resources/manual-test/")
      .entries
      .map(_.name)
      .filter(_.endsWith(".zip"))
      .toList
  }
}
