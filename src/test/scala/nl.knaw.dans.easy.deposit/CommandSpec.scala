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

import nl.knaw.dans.easy.deposit.Errors.NoSuchDepositException
import nl.knaw.dans.easy.deposit.docs.StateInfo

import scala.util.Failure

class CommandSpec extends TestSupportFixture {
  System.setProperty("app.home", "src/main/assembly/dist") // Use the default settings in this test

  private def cmdLine(args: Array[String]) = {
    val cmd = new CommandLineOptions(args, minimalAppConfig){
      // avoids System.exit() in case of invalid arguments or "--help"
      //override def verify(): Unit = { verified = true }
    }.changeState
    val app = new EasyDepositApiApp(minimalAppConfig)
    app.forceChangeState(
      cmd.draftOwnerId(),
      cmd.draftDepositId(),
      StateInfo(cmd.state(), cmd.description()),
      cmd.doUpdate(),
    )
  }

  "forceChangeState" should "report a missing deposit" in {
    cmdLine(Array("change-state",
      "-l", "SUBMITTED",
      "-d", "submitted by administrator",
      defaultUserInfo.id, uuid.toString),
    ) should matchPattern{
      case Failure(e: NoSuchDepositException) if e.getMessage == s"Deposit $uuid not found" =>
    }
  }

  it should "report a wrong label" in {
    cmdLine(Array("change-state",
      "-l", "SUBSMITTED",
      "-d", "submitted by administrator",
      defaultUserInfo.id, uuid.toString),
    ) should matchPattern{
      case Failure(e: NoSuchDepositException) if e.getMessage == s"Deposit $uuid not found" =>
    }
  }
}
