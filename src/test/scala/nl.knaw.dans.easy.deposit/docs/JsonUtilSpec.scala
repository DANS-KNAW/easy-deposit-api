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

import scala.reflect.runtime.universe.typeOf

class JsonUtilSpec extends TestSupportFixture {

  "enum values" should "be unique across all defined enums" in {
    // see https://github.com/json4s/json4s/issues/142

    val values = JsonUtil.enumerations.flatMap(_.values.map(_.toString))
    values shouldBe values.distinct
  }

  "JsonUtil.enumerations" should "should have all enums defined in jspn objects" in {
    val types = Seq(
      typeOf[DatasetMetadata],
      typeOf[DepositInfo],
      typeOf[StateInfo],
      typeOf[UserInfo]
    )
    val definedEnumerations = types.flatMap(_.companion.decls
      .filter(_.typeSignature <:< typeOf[Enumeration])
      .map(_.name.toString))

    val registeredEnumerations = JsonUtil.enumerations
      .map(_.getClass.getSimpleName.stripSuffix("$"))

    registeredEnumerations should contain allElementsOf definedEnumerations
  }
}
