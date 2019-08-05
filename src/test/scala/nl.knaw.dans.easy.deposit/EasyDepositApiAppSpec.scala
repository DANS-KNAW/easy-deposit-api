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

import java.util.UUID

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC

import scala.util.Success

class EasyDepositApiAppSpec extends TestSupportFixture {
  private val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
    override val pidRequester: PidRequester = mockPidRequester
  }

  private val defaultUser: String = "user001"

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "getDeposits" should "list the deposits of a user in sorted order" in {
    val deposit1 = createDeposit(State.rejected, 1)
    val deposit2 = createDeposit(State.archived, 2)
    val deposit3 = createDeposit(State.rejected, 3)
    val deposit4 = createDeposit(State.draft, 4)
    val deposit5 = createDeposit(State.inProgress, 5)
    val deposit6 = createDeposit(State.archived, 6)
    val deposit7 = createDeposit(State.archived, 6, user = "user002") // this one should not show up in the listing!

    // copy from draft to submit directory: deposit5's state will be read out of the submit directory
    copyDepositToSubmitArea(deposit5.id)

    inside(app.getDeposits(defaultUser)) {
      case Success(sortedDeposits) =>
        sortedDeposits should contain inOrderOnly(
          // first rejected deposits (newest to oldest)
          deposit3,
          deposit1,
          // then draft deposits
          deposit4,
          // then in-progress deposits
          deposit5.copy(stateDescription = s"""Something went wrong while processing this deposit. Please <a href="mailto:info@dans.knaw.nl?subject=my-title%20%28reference:%20${deposit5.id}%29&body=Dear%20data%20manager%2C%0A%0ASomething%20went%20wrong%20while%20processing%20my%20deposit.%20Could%20you%20please%20look%20into%20what%20went%20wrong%20with%20it?%0A%0AIt%20has%20reference:%0A%20%20%20${deposit5.id}%0Aand%20title:%0A%20%20%20my-title%0A%0AKind%20regards%2C%0Auser001%0A">contact DANS</a>"""),
          // and finally archived deposits (newest to oldest)
          deposit6,
          deposit2,
        )

        sortedDeposits should not contain deposit7
    }
  }

  private def createDeposit(state: State, createdDaysAgo: Int, user: String = defaultUser, title: String = "my-title"): DepositInfo = {
    val depositId = UUID.randomUUID()
    val deposit = (testDir / "drafts" / user / depositId.toString).createDirectories()
    val creationTimestamp = DateTime.now.minusDays(createdDaysAgo).withZone(UTC)

    val datasetJson =
      s"""{
         |  "titles": [
         |    "$title"
         |  ]
         |}""".stripMargin
    ((deposit / bagDirName / "metadata").createDirectories() / "dataset.json").writeText(datasetJson)

    new PropertiesConfiguration() {
      setProperty("state.label", state)
      setProperty("state.description", "my-description")
      setProperty("creation.timestamp", creationTimestamp)
      setProperty("bag-store.bag-id", depositId)

      save((deposit / "deposit.properties").toJava)
    }

    DepositInfo(depositId, title, state, "my-description", creationTimestamp)
  }

  private def copyDepositToSubmitArea(depositId: UUID, user: String = defaultUser): Unit = {
    testDir / "drafts" / user / depositId.toString copyTo testDir / "easy-ingest-flow-inbox" / depositId.toString
  }
}
