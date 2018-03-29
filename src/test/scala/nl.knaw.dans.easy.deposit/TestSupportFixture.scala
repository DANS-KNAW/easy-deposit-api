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
import better.files.File._
import org.scalamock.scalatest.MockFactory
import org.scalatest._

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfter {

  lazy val testDir: File = {
    (currentWorkingDirectory / "target" / "test" / getClass.getSimpleName)
      .delete(true)
      .createDirectories()
  }
}
