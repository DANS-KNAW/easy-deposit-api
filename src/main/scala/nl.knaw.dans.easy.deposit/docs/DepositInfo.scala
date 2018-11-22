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

import nl.knaw.dans.easy.deposit.docs.DepositInfo._
import nl.knaw.dans.easy.deposit.docs.JsonUtil.RichJsonInput
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.easy.deposit.docs.StateInfo.State.State
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonInput

import scala.util.Try

/**
 * Summary information about a deposit.
 *
 * @param title            [[nl.knaw.dans.easy.deposit.DepositDir#getDepositInfo]] retrieves it from [[DatasetMetadata]]
 * @param state            [[nl.knaw.dans.easy.deposit.DepositDir#create]] stores it in deposit.properties
 * @param stateDescription stored in deposit.properties
 * @param date             stored with gov.loc.repository.bagit.creator.BagCreator
 *                         and as creation.timestamp in deposit.properties
 */
case class DepositInfo(id: UUID = UUID.randomUUID(),
                       title: String = "",
                       state: State = State.draft,
                       stateDescription: String = "Deposit is open for changes.",
                       date: DateTime = nowWithoutMillis
                      ) {

  def timestampString: String = date.toString(dateTimeFormatter)
}
object DepositInfo {
  def apply(input: JsonInput): Try[DepositInfo] = input.deserialize[DepositInfo]

  private def nowWithoutMillis: DateTime = {
    val now = DateTime.now(UTC)
    now.minusMillis(now.millisOfSecond().get())
  }

  private val dateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis()
}
