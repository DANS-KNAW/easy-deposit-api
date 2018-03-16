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
package nl.knaw.dans.easy.deposit.deposit

import java.util.UUID

import scala.util.Try

trait DepositComponent {
  val deposit: Deposit

  trait Deposit {

    /**
     * Creates a new, empty deposit, containing an empty bag in the user's draft area. If the user
     * has no draft area yet, it is first created.
     *
     * @param user the user ID
     * @return
     */
    def createDeposit(user: String): Try[UUID] = ???


    /**
     * Returns the list of `DepositInfo` objects for the deposits in the user's draft area.
     *
     * @param user the user ID
     * @return
     */
    def getDeposits(user: String): Try[Seq[DepositInfo]] = ???


    /**
     * Returns the current state of the deposit.
     *
     * @param user
     * @return
     */
    def getDepositState(user: String, id: UUID): Try[StateInfo] = ???


    /**
     * Deletes the deposit from the user's draft area.
     *
     * @param user the user ID
     * @param id the deposit ID
     * @return
     */
    def deleteDeposit(user: String, id: UUID): Try[Unit] = ???


  }
}
