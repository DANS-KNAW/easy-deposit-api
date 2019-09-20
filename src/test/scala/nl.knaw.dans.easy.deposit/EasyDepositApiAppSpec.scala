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

import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC

import scala.util.Success

class EasyDepositApiAppSpec extends TestSupportFixture {
  private val app: EasyDepositApiApp = createTestApp

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

    // the state is required for the pattern match but the description is error prone and beyond the scope of the test
    val expectedState = app.getDepositState(defaultUser, deposit5.id).getOrElse(fail())

    val testResult = app.getDeposits(defaultUser)
    inside(testResult) {
      case Success(sortedDeposits) =>
        sortedDeposits should contain inOrderOnly(
          // first rejected deposits (newest to oldest)
          deposit3,
          deposit1,
          // then draft deposits
          deposit4,
          // then in-progress deposits
          deposit5.copy(state = expectedState.state, stateDescription = expectedState.stateDescription),
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
