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

import java.io
import java.util.Base64

import better.files.File
import nl.knaw.dans.easy.deposit.authentication.AuthenticationProvider
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.easy.deposit.{ EasyDepositApiApp, TestSupportFixture }
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalatra.test.EmbeddedJettyContainer
import org.scalatra.test.scalatest.ScalatraSuite

trait ServletFixture extends TestSupportFixture with EmbeddedJettyContainer with ScalatraSuite {
  this: ScalatraSuite =>

  def mountServlets(app: EasyDepositApiApp, authenticationProvider: AuthenticationProvider): Unit = {
    val userServlet = new UserServlet(app) with UndoMasking {
      override def getAuthenticationProvider: AuthenticationProvider = authenticationProvider
    }
    val authServlet = new AuthServlet(app) with UndoMasking {
      override def getAuthenticationProvider: AuthenticationProvider = authenticationProvider
    }
    val appServlet = new EasyDepositApiServlet(app) with UndoMasking
    mountDepositServlet(app, authenticationProvider)
    addServlet(userServlet, "/user/*")
    addServlet(authServlet, "/auth/*")
    addServlet(appServlet, "/*")
  }

  def mountDepositServlet(app: EasyDepositApiApp, authenticationProvider: AuthenticationProvider): Unit = {
    val depositServlet = new DepositServlet(app) with UndoMasking {
      override def getAuthenticationProvider: AuthenticationProvider = authenticationProvider
    }
    addServlet(depositServlet, "/deposit/*")
  }

  /**
   * Mimics field values for: `<form method="post" enctype="multipart/form-data">`
   *
   * @param files tuples of:
   *              - name in `<input type="file" name="some">`
   *              - simple file name
   *              - content of the file to create
   * @return
   */
  def bodyParts(dir: File, files: Seq[(String, String, String)]): Seq[(String, io.File)] = {
    dir.createDirectories()
    files.map { case (formField, file, content) =>
      (dir / file).write(content)
      (formField, (dir / file).toJava)
    }
  }

  val fooBarBasicAuthHeader: (String, String) = {
    val encoded = Base64.getEncoder.encodeToString("foo:bar".getBytes())
    ("Authorization", s"Basic $encoded")
  }

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
}
