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

import nl.knaw.dans.easy.deposit.EasyDepositApiApp
import nl.knaw.dans.easy.deposit.authentication._
import nl.knaw.dans.easy.deposit.logging.{ RequestLogger, ResponseLogger }
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatra.ScalatraServlet

abstract class AbstractAuthServlet(app: EasyDepositApiApp) extends ScalatraServlet
  with RequestLogger
  with ResponseLogger
  with AuthenticationSupport
  with TokenSupport
  with AuthConfig {

  override def getAuthenticationProvider: AuthenticationProvider = app.authentication

  override def getProperties: PropertiesConfiguration = app.properties
}
