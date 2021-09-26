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
package nl.knaw.dans.easy.deposit.authentication

import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState
import nl.knaw.dans.easy.deposit.authentication.AuthUser.UserState.UserState
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory

import scala.util.{ Success, Try }

trait AuthenticationMocker extends MockFactory {
  val mockedAuthenticationProvider: AuthenticationProvider

  def expectsInvalidUser: CallHandler2[String, String, Try[Option[AuthUser]]] = {
    (mockedAuthenticationProvider.authenticate(_: String, _: String)) expects(*, *) returning Success(None)
  }

  def expectsUserFooBar: CallHandler2[String, String, Try[Option[AuthUser]]] = {
    (mockedAuthenticationProvider.authenticate(_: String, _: String)) expects("foo", "bar") returning
      Success(Some(AuthUser("foo", state = UserState.active)))
  }

  def expectsUserFooBarWithStatus(userState: UserState): CallHandler2[String, String, Try[Option[AuthUser]]] = {
    (mockedAuthenticationProvider.authenticate(_: String, _: String)) expects("foo", "bar") returning
      Success(Some(AuthUser("foo", state = userState)))
  }

  def expectsNoUser: CallHandler2[String, String, Try[Option[AuthUser]]] = {
    (mockedAuthenticationProvider.authenticate(_: String, _: String)) expects(*, *) never()
  }
}
object AuthenticationMocker {
  def expectsUserFooBarAnyNumberOfTimes: AuthenticationProvider = new AuthenticationMocker() {
    override val mockedAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]
    expectsUserFooBar anyNumberOfTimes()
  }.mockedAuthenticationProvider
}
