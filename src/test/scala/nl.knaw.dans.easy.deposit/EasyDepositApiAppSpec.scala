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

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "getDeposits" should "list the deposits of a user in sorted order" in {
    val deposit1 = createDeposit("foo", "my-title1", State.rejected, DateTime.now.minusDays(1).withZone(UTC))
    val deposit2 = createDeposit("foo", "my-title2", State.archived, DateTime.now.minusDays(2).withZone(UTC))
    val deposit3 = createDeposit("foo", "my-title3", State.rejected, DateTime.now.minusDays(3).withZone(UTC))
    val deposit4 = createDeposit("foo", "my-title4", State.draft, DateTime.now.minusDays(4).withZone(UTC))
    val deposit5 = createDeposit("foo", "my-title5", State.inProgress, DateTime.now.minusDays(5).withZone(UTC))
    val deposit6 = createDeposit("foo", "my-title6", State.archived, DateTime.now.minusDays(6).withZone(UTC))

    // copy from draft to submit directory: deposit5's state will be read out of the submit directory
    copyDepositToSubmitArea("foo", deposit5.id)

    inside(app.getDeposits("foo")) {
      case Success(sortedDeposits) =>
        sortedDeposits should contain inOrderOnly(
          // first rejected deposits (newest to oldest)
          deposit3,
          deposit1,
          // then draft deposits
          deposit4,
          // then in-progress deposits
          deposit5.copy(stateDescription = "The deposit is in progress."),
          // and finally archived deposits (newest to oldest)
          deposit6,
          deposit2,
        )
    }
  }

  private def createDeposit(user: String, title: String, state: State, creationTimestamp: DateTime): DepositInfo = {
    val depositId = UUID.randomUUID()
    val deposit = (testDir / "drafts" / user / depositId.toString).createDirectories()

    val datasetJson =
      s"""{
         |  "titles": [
         |    "$title"
         |  ]
         |}""".stripMargin
    ((deposit / "bag" / "metadata").createDirectories() / "dataset.json").writeText(datasetJson)

    new PropertiesConfiguration() {
      setProperty("state.label", state)
      setProperty("state.description", "my-description")
      setProperty("creation.timestamp", creationTimestamp)
      setProperty("bag-store.bag-id", depositId)

      save((deposit / "deposit.properties").toJava)
    }

    DepositInfo(depositId, title, state, "my-description", creationTimestamp)
  }

  private def copyDepositToSubmitArea(user: String, depositId: UUID): Unit = {
    testDir / "drafts" / user / depositId.toString copyTo testDir / "easy-ingest-flow-inbox" / depositId.toString
  }
}
