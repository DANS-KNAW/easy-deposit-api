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
package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.TestSupportFixture
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig

import scala.util.Success

class TokenSupportSpec extends TestSupportFixture with TokenSupport {

  val getTokenConfig: TokenConfig = testTokenConfig

  "toUser" should "return encoded value" in {
    val user = AuthUser("foo", isActive = true)
    decodeJWT(encodeJWT(user)) shouldBe Success(user)
  }
}