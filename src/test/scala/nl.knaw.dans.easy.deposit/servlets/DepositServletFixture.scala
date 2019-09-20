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
package nl.knaw.dans.easy.deposit.servlets

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import nl.knaw.dans.easy.deposit.PidRequesterComponent.{PidRequester, PidType}
import nl.knaw.dans.easy.deposit.authentication.{AuthenticationMocker, AuthenticationProvider}
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.easy.deposit.{EasyDepositApiApp, TestSupportFixture}
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.handlers.CallHandler1
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

import scala.util.{Success, Try}

trait DepositServletFixture extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {
  private val app: EasyDepositApiApp = createTestApp
  private val depositServlet = new DepositServlet(app) with UndoMasking {
    override def getAuthenticationProvider: AuthenticationProvider = {
      new AuthenticationMocker() {
        override val mockedAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]
        expectsUserFooBar anyNumberOfTimes()
      }.mockedAuthenticationProvider
    }
  }
  addServlet(depositServlet, "/deposit/*")

  /** @return UUID of the created deposit */
  def createDeposit: String = {
    post("/deposit/", headers = Seq(fooBarBasicAuthHeader)) {
      // prevent json to interpret a stack trace
      status shouldBe CREATED_201

      // return the new UUID
      DepositInfo(body)
        .map(_.id.toString)
        .getOrRecover(e => fail(e.toString, e))
    }
  }

  def mockDoiRequest(doi: String): CallHandler1[PidType, Try[String]] =
    (app.pidRequester.requestPid(_: PidType)) expects PidType.doi returning Success(doi)
}
