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

import nl.knaw.dans.easy.deposit._
import org.joda.time.{ DateTime, DateTimeZone }

import scala.util.Success

class DepositInfoSpec extends TestSupportFixture {

  private val now = "2018-03-22T21:43:01.576"
  private val nowUTC = "2018-03-22T20:43:01Z"
  private val strict = true
  mockDateTimeNow(now)
  DateTimeZone.getDefault.convertLocalToUTC(new DateTime(now).getMillis, strict)
  val depositInfoObject = DepositInfo(title = "Lorum ipsum")
  val depositInfoString = s"""{"id":"$uuid","title":"Lorum ipsum","state":"DRAFT","stateDescription":"Deposit is open for changes.","date":"2018-03-22T21:43:01Z"}"""

  "serialization/deserialisation" should "return the same object" in {
    DepositInfo(JsonUtil.toJson(depositInfoObject)) shouldBe Success(depositInfoObject.copy(date = depositInfoObject.date))
  }

  "deserialization/serialisation" should "return the same string" in {
    DepositInfo(depositInfoString).map(JsonUtil.toJson) shouldBe Success(depositInfoString)
  }
}
