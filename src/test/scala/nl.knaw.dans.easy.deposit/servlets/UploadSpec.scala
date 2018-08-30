package nl.knaw.dans.easy.deposit.servlets

import java.util.UUID

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidRequester
import nl.knaw.dans.easy.deposit.authentication.AuthenticationMocker.{ expectsUserFooBar, mockedAuthenticationProvider }
import nl.knaw.dans.easy.deposit.docs.DepositInfo
import nl.knaw.dans.easy.deposit.{ DepositDir, EasyDepositApiApp, TestSupportFixture }
import nl.knaw.dans.lib.error._
import org.eclipse.jetty.http.HttpStatus._
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class UploadSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
    (testDir / "drafts").createDirectories()
    (testDir / "input").createDirectory()
  }

  private val basicAuthentication: (String, String) = ("Authorization", fooBarBasicAuthHeader)
  private val app: EasyDepositApiApp = new EasyDepositApiApp(minimalAppConfig) {
    override val pidRequester: PidRequester = null
  }
  mountServlets(app, mockedAuthenticationProvider)

  s"POST" should "upload files from multiple form fields" in {
    val files = {
      Seq(
        ("1.txt", "Lorem ipsum dolor sit amet"),
        ("2.txt", "consectetur adipiscing elit"),
        ("3.txt", "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"),
        ("4.txt", "Ut enim ad minim veniam")
      ).foreach { case (name, content) => (testDir / "input" / name).write(content) }

      // field values for: <form method="post" enctype="multipart/form-data">
      Iterable(
        // <input type="file" name="some">
        ("some", (testDir / "input/1.txt").toJava),
        ("some", (testDir / "input/2.txt").toJava),
        // <input type="file" name="others">
        ("others", (testDir / "input/3.txt").toJava),
        ("others", (testDir / "input/4.txt").toJava),
      )
    }
    val uuid = createDataset
    val relativeTarget = "path/to/dir"
    expectsUserFooBar
    post(
      uri = s"/deposit/$uuid/file/$relativeTarget",
      params = Iterable(),
      headers = Seq(basicAuthentication),
      files = files
    ) {
      body shouldBe ""
      status shouldBe OK_200
      val uploaded = (testDir / "drafts/foo" / uuid.toString / "bag/data" / relativeTarget).list
      uploaded.size shouldBe files.size
      uploaded.foreach(file =>
        file.contentAsString shouldBe (testDir / "input" / file.name).contentAsString
      )
    }
  }

  s"PUT" should "return 201 for a new respectively 200 for a replaced file" in {
    val uuid = createDataset

    val bagBase = DepositDir(testDir / "drafts", "foo", UUID.fromString(uuid.toString)).getDataFiles.get.bag
    val shortContent = "Lorum ipsum"
    val longContent = "dolor sit amet"

    // first upload
    expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(basicAuthentication),
      body = longContent
    ) {
      status shouldBe CREATED_201
      (bagBase / "data/path/to/text.txt").contentAsString shouldBe longContent
      (bagBase / "manifest-sha1.txt").contentAsString shouldBe "0d21d1af59b36f0bee70fd034e931ec72f04f1cd  data/path/to/text.txt\n"
    }

    // second upload of same file
    expectsUserFooBar
    put(
      uri = s"/deposit/$uuid/file/path/to/text.txt",
      headers = Seq(basicAuthentication),
      body = shortContent
    ) {
      status shouldBe OK_200
      (bagBase / "data/path/to/text.txt").contentAsString shouldBe shortContent
      (bagBase / "manifest-sha1.txt").contentAsString shouldBe "c5b8de8cc3587aef4e118a481115391033621e06  data/path/to/text.txt\n"
    }
  }

  private def createDataset: UUID = {
    expectsUserFooBar
    val responseBody = post(
      uri = s"/deposit",
      headers = Seq(basicAuthentication)
    ) {
      new String(bodyBytes)
    }
    DepositInfo(responseBody)
      .map(_.id)
      .getOrRecover(e => fail(s"preparing a dataset failed: $e", e))
  }
}
