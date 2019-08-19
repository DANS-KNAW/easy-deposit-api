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
