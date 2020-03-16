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

import java.nio.file.Paths

class CommandSpec extends TestSupportFixture {
  System.setProperty("app.home", "src/main/assembly/dist") // Use the default settings in this test

  private TestCommand
  private def cmdLine(args: Array[String]) = {
    new CommandLineOptions(args, Configuration(Paths.get("src/main/assembly/dist"))) {
      // avoids System.exit() in case of invalid arguments or "--help"
      override def verify(): Unit = {}
    }
  }
  "changeState" should "" in {
    Command.runSubcommand()
    cmdLine(Array("blabla")).changeState shouldBe 1
  }
}
