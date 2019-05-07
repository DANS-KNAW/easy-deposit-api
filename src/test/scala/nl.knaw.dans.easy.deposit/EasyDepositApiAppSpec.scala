package nl.knaw.dans.easy.deposit

import java.util.UUID

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.docs.DepositInfo

import scala.util.Success

class EasyDepositApiAppSpec extends TestSupportFixture {
  private val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
    override val pidRequester: PidRequester = mockPidRequester
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "getDeposits" should "" in {
    createDeposit(""" """.stripMargin)
    app.getDeposits("foo") should matchPattern {
      case Success(Seq(DepositInfo(_,_,_,_,_))) =>
    }
  }

  private def createDeposit(props: String) = {
    val deposit = (testDir / "drafts" / "foo" / UUID.randomUUID().toString).createDirectories()
    ((deposit / "bag" / "metadata").createDirectories() / "dataset.json").writeText("{}")
    (deposit / "deposit.properties").writeText(props)
  }
}
