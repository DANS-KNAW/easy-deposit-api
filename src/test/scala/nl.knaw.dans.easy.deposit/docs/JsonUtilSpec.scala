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

import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.{ DateQualifier, DateScheme }
import org.joda.time.DateTime

import scala.util.{ Success, Try }

class JsonUtilSpec extends TestSupportFixture {

  "enum values" should "be unique across all defined enums" in {
    // see https://github.com/json4s/json4s/issues/142

    val values = JsonUtil.enumerations.flatMap(_.values.map(_.toString))
    values shouldBe values.distinct
  }

  "JsonUtil.enumerations" should "should have all enums defined in DatasetMetatadata" ignore {
    val enums: List[Enumeration] = ??? // TODO DatasetMetadata.enums
    val checkedEnums = enums.map { e =>
      (e, JsonUtil.enumerations.contains(e))
    }
//    every(checkedEnums) shouldBe fullyMatch {
//      case (_, true) =>
//    }
  }
}
