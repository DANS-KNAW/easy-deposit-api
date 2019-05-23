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
package nl.knaw.dans.easy.deposit

import javax.servlet.ServletContext
import nl.knaw.dans.easy.deposit.servlets._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.eclipse.jetty.server.handler.{ DefaultHandler, HandlerCollection, RequestLogHandler }
import org.eclipse.jetty.server.{ Server, Slf4jRequestLog }
import org.eclipse.jetty.servlet.ServletContextHandler
import org.scalatra._
import org.scalatra.servlet.ScalatraListener

import scala.util.Try


class EasyDepositApiService(serverPort: Int, app: EasyDepositApiApp) extends DebugEnhancedLogging {

  import logger._

  private val server: Server = new Server(serverPort) {
    private val context = new ServletContextHandler(ServletContextHandler.NO_SESSIONS & ServletContextHandler.SECURITY) {
      addEventListener(new ScalatraListener() {
        override def probeForCycleClass(classLoader: ClassLoader): (String, LifeCycle) = {
          ("anonymous", new LifeCycle {
            override def init(context: ServletContext): Unit = {
              context.mount(new EasyDepositApiServlet(app), "/*")
              context.mount(new DepositServlet(app), "/deposit/*")
              context.mount(new UserServlet(app), "/user/*")
              context.mount(new AuthServlet(app), "/auth/*")
            }
          })
        }
      })
    }
    // TODO https://logback.qos.ch/recipes/captureHttp.html
    //  see also
    //  https://logback.qos.ch/access.html#jetty
    //  https://www.eclipse.org/jetty/documentation/current/configuring-jetty-request-logs.html
    private val requestLogHandler = new RequestLogHandler {
      setRequestLog(new Slf4jRequestLog() {
        setExtended(true)
        setLogTimeZone("GMT")
        setDumpAfterStart(true)
        setLogCookies(false)
        setLogServer(true)
      })
    }
    setHandler(new HandlerCollection {
      setHandlers(Array(context, new DefaultHandler(), requestLogHandler))
    })
  }

  info(s"HTTP port is ${ serverPort }")

  def start(): Try[Unit] = Try {
    info("Starting service...")
    server.start()
  }

  def stop(): Try[Unit] = Try {
    info("Stopping service...")
    server.stop()
  }

  def destroy(): Try[Unit] = Try {
    server.destroy()
  }
}
