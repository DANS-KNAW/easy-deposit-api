package nl.knaw.dans.easy.deposit

import java.util.UUID.randomUUID

import nl.knaw.dans.easy.deposit.WorkerThread.apiApp
import nl.knaw.dans.easy.deposit.docs.StateInfo
import org.scalamock.scalatest.MockFactory

class WorkerThreadSpec extends TestSupportFixture with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    System.setProperty("app.home", testDir.toString())
    val config = minimalAppConfig
    config.properties.save(((testDir / "cfg").createDirectories() / "application.properties").toJava)
    ((testDir / "bin").createDirectories() / "version").write(config.version)
    WorkerThread.apiApp.stagedBaseDir.createDirectories()
  }

  "handleWatchService" should "log some warnings during start up" in {
    val watchedDir = apiApp.stagedBaseDir
    (watchedDir / "garbage").createDirectories()
    (watchedDir / "foo-123-4").createDirectories()
    (watchedDir / s"bar-${ randomUUID() }-567").createDirectories()
    (watchedDir / s"somebody-${ createTestDeposit("somebody") }-567").createDirectories()
    val user = "someone"
    val uuid = createTestDeposit(user)
    val thread = new Thread {
      override def run() {
        WorkerThread.main(Array[String]())
      }
    }
    thread.start()
    println(s"${ thread.getState } ")
    Thread.sleep(500) // TODO figure out when startup phase is completed (empty staged dir? not while handlers are stubs)
    val stateInfo = new StateInfo(StateInfo.State.submitted, "")
    apiApp.setDepositState(stateInfo, user, uuid) // TODO expected to be logged as "Detected submit event" after startup
    Thread.sleep(1500) // TODO figure out when/how to terminate thread, intercept log of thread
    println(s"${ thread.getState } ")
  }

  private def createTestDeposit(someone: String) = {
    apiApp.createDeposit(someone).get.id // TODO unsafe get
  }
}
