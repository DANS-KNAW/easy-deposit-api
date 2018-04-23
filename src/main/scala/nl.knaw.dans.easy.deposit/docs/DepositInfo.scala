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
package nl.knaw.dans.easy.deposit.docs

import java.util.UUID

import nl.knaw.dans.easy.deposit.State
import nl.knaw.dans.easy.deposit.State.State
import nl.knaw.dans.easy.deposit.docs.DepositInfo._
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC
import org.joda.time.format.ISODateTimeFormat

/**
 * Summary information about a deposit.
 *
 * @param title [[../DepositDir#getDepositInfo]] retrieves it form [[DatasetMetadata]]
 * @param state [[../DepositDir#create]] stores it in deposit.properties
 * @param stateDescription stored in deposit.properties
 * @param date             stored in [[../BagitMetadata]].Created
 *                         and as creation.timestamp in deposit.properties
 */
case class DepositInfo(id: UUID = UUID.randomUUID(),
                       title: String = "",
                       state: State = State.DRAFT,
                       stateDescription: String = "Deposit is open for changes.",
                       date: DateTime = nowWithoutMillis
                      ) {

  def timestampString: String = date.toString(dateTimeFormatter)
}
object DepositInfo {
  def nowWithoutMillis: DateTime = {
    val now = DateTime.now(UTC)
    now.minusMillis(now.millisOfSecond().get())
  }

  private val dateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis()
}
