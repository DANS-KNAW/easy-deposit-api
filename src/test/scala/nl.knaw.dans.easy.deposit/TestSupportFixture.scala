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

import java.net.{ URI, URL }
import java.util.concurrent.ThreadPoolExecutor
import java.util.{ TimeZone, UUID }

import better.files.File
import better.files.File._
import javax.activation.DataSource
import nl.knaw.dans.easy.deposit.authentication.TokenSupport.TokenConfig
import nl.knaw.dans.easy.deposit.authentication.{ AuthConfig, AuthUser, AuthenticationProvider, TokenSupport }
import nl.knaw.dans.easy.deposit.docs.{ AgreementData, UserData }
import nl.knaw.dans.easy.deposit.executor.{ JobQueueManager, SystemStatus }
import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.mail.MultiPartEmail
import org.joda.time.{ DateTime, DateTimeUtils, DateTimeZone }
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.enablers.Existence
import org.slf4j.bridge.SLF4JBridgeHandler
import scalaj.http.Http

import scala.util.{ Properties, Success, Try }

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfterEach with MockFactory {

  // disable logs from okhttp3.mockwebserver
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()

  implicit def existenceOfFile[FILE <: better.files.File]: Existence[FILE] = _.exists

  lazy val testDir: File = currentWorkingDirectory / "target" / "test" / getClass.getSimpleName
  lazy val uuid: UUID = UUID.randomUUID()
  val bagDirName = "bag"

  // modification of https://github.com/DANS-KNAW/easy-split-multi-deposit/blob/ea7c2dc3d6/src/test/scala/nl.knaw.dans.easy.multideposit/actions/SetDepositPermissionsSpec.scala#L102
  lazy val (userGroup, unrelatedGroup) = {
    import scala.sys.process._

    // don't hardcode users and groups, since we don't know what we have on travis or personal systems
    val user = Properties.userName
    val allGroups = "cut -d: -f1 /etc/group".!!.split("\n").filterNot(_ startsWith "#").toList
    val userGroups = s"id -Gn $user".!!.split(" ").toList

    (userGroups, allGroups.diff(userGroups)) match {
      case (ug :: _, diff :: _) => (ug, diff)
      case (Nil, _) => throw new AssertionError("no suitable user group found")
      case (_, Nil) => throw new AssertionError("no suitable unrelated group found")
    }
  }

  val defaultUserInfo: UserData = UserData(
    id = "user001",
    name = "fullName",
    firstName = None,
    prefix = None,
    lastName = "",
    address = "",
    zipcode = "",
    city = "",
    country = "",
    organisation = "",
    phone = "",
    email = "does.not.exist@dans.knaw.nl",
  )

  def testResource(file: String): File = File(getClass.getResource(file))

  def getManualTestResource(file: String): String = {
    (testResource("/manual-test") / file).contentAsString
  }

  def clearTestDir(): Unit = {
    if (testDir.exists)
      testDir.delete().createDirectories()
  }

  val nowYMD = "2018-03-22"
  val now = s"${ nowYMD }T21:43:01.576"
  val nowUTC = s"${ nowYMD }T20:43:01Z"
  /** Causes DateTime.now() to return a predefined value. */
  DateTimeUtils.setCurrentMillisFixed(new DateTime(nowUTC).getMillis)
  DateTimeZone.setDefault(DateTimeZone.forTimeZone(TimeZone.getTimeZone("Europe/Amsterdam")))

  class MockedPidRequester extends PidRequester(Http, new URI("http://does.not.exist.dans.knaw.nl/pid-generator"))

  def minimalAppConfig: Configuration = {
    new Configuration("", new PropertiesConfiguration() {
      addProperty("deposits.staged", testSubDir("staged").toString())
      addProperty("deposits.drafts", testSubDir("drafts").toString())
      addProperty("deposits.submit-to", testSubDir("easy-ingest-flow-inbox").toString())
      addProperty("deposit.permissions.group", userGroup)
      addProperty("pids.generator-service", "http://pidHostDoesNotExist.dans.knaw.nl")
      addProperty("users.ldap-url", "ldap://ldapHostDoesNotExist.dans.knaw.nl")
      addProperty("users.ldap-parent-entry", "-")
      addProperty("users.ldap-admin-principal", "-")
      addProperty("users.ldap-admin-password", "-")
      addProperty("users.ldap-user-id-attr-name", "-")
      addProperty("multipart.location", (testDir / "multipart").createDirectories().toString())
      addProperty("multipart.file-size-threshold", "3145728") // 3MB
      addProperty("easy.home", "https://easy.dans.knaw.nl/ui")
      addProperty("threadpool.core-pool-size", 1)
      addProperty("threadpool.max-pool-size", 2)
      addProperty("threadpool.keep-alive-time-ms", 60000L)
      addProperty("easy.home", "https://doesNotExist.dans.knaw.nl/ui")
      addProperty("easy.my-datasets", "https://doesNotExist.dans.knaw.nl/ui/mydatasets")
      // lazy values in the mailer would require less parameters here,
      // but without lazy the service fails fast when started with an invalid configuration
      addProperty("mail.smtp.host", "http://mailerDoesNotExist.dans.knaw.nl")
      addProperty("mail.fromAddress", "does.not.exist@dans.knaw.nl")
      addProperty("mail.template", "src/main/assembly/dist/cfg/template")
      addProperty("attached-file-list.limit", "500")
      addProperty("agreement-generator.url", "http://agreementGeneratorDoesNotExist.dans.knaw.nl")
      addProperty("agreement-generator.connection-timeout-ms", "3000")
      addProperty("agreement-generator.read-timeout-ms", "60000")
    })
  }

  def createTestApp(requester: PidRequester): EasyDepositApiApp = {
    new EasyDepositApiApp(minimalAppConfig) {
      override val pidRequester: PidRequester = requester

      override def getUserData(user: String): Try[UserData] = {
        Success(defaultUserInfo)
      }

      override protected val submitter: Submitter = {
        val group = properties.getString("deposit.permissions.group")
        new Submitter(
          submitToBaseDir = submitBase,
          groupPrincipal = submitBase.fileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(group),
          depositUiURL = properties.getString("easy.deposit-ui"),
          jobQueue = jobQueueStub(),
          mailer = mailerStub,
          agreementGenerator = agreementGeneratorStub,
        )
      }
    }
  }

  private def jobQueueStub(onSchedule: SubmitJob => Try[Unit] = (job: SubmitJob) => Try(job.run()),
                          ): JobQueueManager = {
    val threadPoolExecutor: ThreadPoolExecutor = null
    new JobQueueManager(threadPoolExecutor) {
      override def getSystemStatus: SystemStatus = {
        SystemStatus(threadPoolStatus = null, queueSize = 0, queueContent = null)
      }

      override def scheduleJob(job: Runnable): Try[Unit] = Try {
        job.run()
      }
    }
  }

  private def agreementGeneratorStub = {
    new AgreementGenerator(Http, new URL("http://does.not.exist"), "text/html") {
      override def generate(agreementData: AgreementData, id: UUID): Try[Array[Byte]] = {
        Success("mocked pdf".getBytes)
      }
    }
  }

  private def mailerStub = {
    new Mailer(
      smtpHost = "",
      fromAddress = "",
      bounceAddress = "",
      bccs = Seq.empty,
      templateDir = File("src/main/assembly/dist/cfg/template"),
      myDatasets = new URL("http://does.not.exist")
    ) {
      override def buildMessage(data: AgreementData, attachments: Map[String, DataSource], depositId: UUID, msg: Option[String]): Try[MultiPartEmail] = {
        Success(new MultiPartEmail) // only cause causes the following logging:
        // ERROR could not send deposit confirmation message
        //java.lang.IllegalArgumentException: MimeMessage has not been created yet
      }
    }
  }

  private def testSubDir(drafts: String): File = {
    (testDir / drafts)
      .delete(swallowIOExceptions = true)
      .createIfNotExists(asDirectory = true, createParents = true)
  }

  private class TokenSupportImpl() extends TokenSupport with AuthConfig {

    // required by AuthConfig but not by TokenSupport
    def getAuthenticationProvider: AuthenticationProvider = mock[AuthenticationProvider]

    def getProperties: PropertiesConfiguration = new PropertiesConfiguration()
  }

  private val tokenSupport = new TokenSupportImpl()

  def jwtConfig: TokenConfig = tokenSupport.tokenConfig

  def createJWT(user: AuthUser): String = {
    tokenSupport.encodeJWT(user)
  }
}
