/*
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
package nl.knaw.dans.easy.deposit.docs

import nl.knaw.dans.easy.deposit.Errors.IllegalDepositStateException
import nl.knaw.dans.easy.deposit.TestSupportFixture

import scala.util.{ Failure, Success }

class StateInfoSpec extends TestSupportFixture {

  "isDeletable" should "succeed when the state is DRAFT" in {
    new StateInfo(StateInfo.State.draft, "drafted").canDelete shouldBe a[Success[_]]
  }

  it should "succeed if the state is archived" in {
    new StateInfo(StateInfo.State.archived, "archived").canDelete shouldBe a[Success[_]]
  }

  it should "succeed if the state is rejected" in {
    new StateInfo(StateInfo.State.archived, "rejected").canDelete shouldBe a[Success[_]]
  }

  it should "fail if the state is submitted" in {
    new StateInfo(StateInfo.State.submitted, "submitted").canDelete should matchPattern {
      case Failure(ise: IllegalDepositStateException) if ise.getMessage == s"Deposit has state SUBMITTED, can only delete deposits with one of the states: DRAFT, ARCHIVED, REJECTED" =>
    }
  }

  it should "fail if the state is in progress" in {
    new StateInfo(StateInfo.State.inProgress, "IN_PROGRESS").canDelete should matchPattern {
      case Failure(ise: IllegalDepositStateException) if ise.getMessage == s"Deposit has state IN_PROGRESS, can only delete deposits with one of the states: DRAFT, ARCHIVED, REJECTED" =>
    }
  }

  "canUpdate" should "fail if the state is rejected" in {
    new StateInfo(StateInfo.State.rejected, "rejected").canUpdate should matchPattern{
      case Failure(ise: IllegalDepositStateException) if ise.getMessage == s"Deposit has state REJECTED, can only update deposits with one of the states: DRAFT" =>
    }
  }

  it should "succeed if the state is draft" in {
    new StateInfo(StateInfo.State.draft, "draft").canUpdate shouldBe a[Success[_]]
  }

  it should "fail if the state is submitted" in {
    new StateInfo(StateInfo.State.submitted, "submitted").canUpdate should matchPattern {
      case Failure(ise: IllegalDepositStateException) if ise.getMessage == s"Deposit has state SUBMITTED, can only update deposits with one of the states: DRAFT" =>
    }
  }

  it should "fail if the state is in progress" in {
    new StateInfo(StateInfo.State.inProgress, "IN_PROGRESS").canUpdate should matchPattern {
      case Failure(ise: IllegalDepositStateException) if ise.getMessage == s"Deposit has state IN_PROGRESS, can only update deposits with one of the states: DRAFT" =>
    }
  }

  it should "fail if the state is archived" in {
    new StateInfo(StateInfo.State.archived, "archived").canUpdate should matchPattern {
      case Failure(ise: IllegalDepositStateException) if ise.getMessage == s"Deposit has state ARCHIVED, can only update deposits with one of the states: DRAFT" =>
    }
  }
}
