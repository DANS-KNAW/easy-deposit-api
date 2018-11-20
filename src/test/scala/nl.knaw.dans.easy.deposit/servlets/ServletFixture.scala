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

import better.files.File
import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.easy.deposit.authentication.AuthenticationProvider
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.scalatra.test.EmbeddedJettyContainer
import org.scalatra.test.scalatest.ScalatraSuite

/**
 * This Suite relies on Jetty 9.x, while we still require Jetty 8.x
 * By overriding localPort and baseUrl below, issues related to these versions are solved.
 */
trait ServletFixture extends EmbeddedJettyContainer {
  this: ScalatraSuite =>

  override def localPort: Option[Int] = server.getConnectors.collectFirst {
    case x: SelectChannelConnector => x.getLocalPort
  }

  override def baseUrl: String = {
    server.getConnectors.collectFirst {
      case conn: SelectChannelConnector =>
        val host = Option(conn.getHost).getOrElse("localhost")
        val port = conn.getLocalPort
        require(port > 0, "The detected local port is < 1, that's not allowed")
        "http://%s:%d".format(host, port)
    }.getOrElse(sys.error("can't calculate base URL: no connector"))
  }

  def mountServlets(app: EasyDepositApiApp, authenticationProvider: AuthenticationProvider): Unit = {
    val userServlet = new UserServlet(app) {
      override def getAuthenticationProvider: AuthenticationProvider = authenticationProvider
    }
    val depositServlet = new DepositServlet(app) {
      override def getAuthenticationProvider: AuthenticationProvider = authenticationProvider
    }
    val authServlet = new AuthServlet(app) {
      override def getAuthenticationProvider: AuthenticationProvider = authenticationProvider
    }
    addServlet(depositServlet, "/deposit/*")
    addServlet(userServlet, "/user/*")
    addServlet(authServlet, "/auth/*")
    addServlet(new EasyDepositApiServlet(app), "/*")
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
}
