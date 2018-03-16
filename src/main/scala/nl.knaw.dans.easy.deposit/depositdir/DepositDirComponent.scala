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
package nl.knaw.dans.easy.deposit.depositdir

import java.util.UUID

import scala.util.Try

trait DepositDirComponent {
  val depositDir: DepositDir

  trait DepositDir {

    /**
     * Creates a new, empty deposit, containing an empty bag in the user's draft area. If the user
     * has no draft area yet, it is first created.
     *
     * @param user the user ID
     * @return the new deposit's ID
     */
    def createDeposit(user: String): Try[UUID] = ???

    /**
     * Returns the list of `DepositInfo` objects for the deposits in the user's draft area.
     *
     * @param user the user ID
     * @return a list of [[DepositInfo]] objects
     */
    def getDeposits(user: String): Try[Seq[DepositInfo]] = ???


    /**
     * Returns the current state of the deposit.
     *
     * @param user ID
     * @param id   the deposit ID
     * @return
     */
    def getDepositState(user: String, id: UUID): Try[StateInfo] = ???


    /**
     * Sets the deposit state. The only legal transitions are:
     *
     * - from [[State.DRAFT]] to [[State.SUBMITTED]]
     * - from [[State.REJECTED]] to [[State.DRAFT]]
     *
     * Any attempt at another transition will result in an [[nl.knaw.dans.easy.deposit.IllegalStateTransitionException]].
     *
     * When transitioning to [[State.SUBMITTED]] the following steps will be executed:
     *
     * 1. The `files.xml` of the bag will be written.
     * 2. The SHA-1 payload manifest will be calculated and written.
     * 3. The deposit directory will be copied to the staging area.
     * 4. The copy will be moved to the deposit area.
     *
     * @param user the user ID
     * @param id the deposit ID
     * @param state the state to transition to
     * @return
     */
    // TODO: do post processing described above in a worker thread so that submit can return fast for large deposits.
    def setDepositState(user: String, id: UUID, state: StateInfo): Try[Unit] = ???

    /**
     * Deletes the deposit from the user's draft area.
     *
     * @param user the user ID
     * @param id   the deposit ID
     * @return
     */
    def deleteDeposit(user: String, id: UUID): Try[Unit] = ???
  }
}
