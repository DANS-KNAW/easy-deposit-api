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

import javax.servlet.http.Part
import nl.knaw.dans.easy.deposit.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import org.scalatra.servlet.FileItem

class RichFileItemsSpec extends TestSupportFixture with MockFactory {

  override def beforeEach(): Unit = {
    super.beforeEach()
    clearTestDir()
  }

  "nextAsZipIfOnlyOne" should "return nothing" in pendingUntilFixed {
    val fileItems = Iterator[FileItem]().buffered
    fileItems.nextAsZipIfOnlyOne shouldBe None
    fileItems.hasNext shouldBe false
  }

  it should "return a zip item" in pendingUntilFixed {
    val mocked = mock[Part]
    mocked.getSize _ expects() returning 20
    mocked.getName _ expects() returning "some.zip"
    mocked.getHeader _ expects * returning null anyNumberOfTimes()
    mocked.getContentType _ expects () returning null
    val fileItems: BufferedIterator[FileItem] = Iterator(FileItem(mocked)).buffered

    fileItems.nextAsZipIfOnlyOne shouldBe ""
    fileItems.hasNext shouldBe false
  }

  it should "return a plain item" in pendingUntilFixed {
    val mocked = mock[Part]
    mocked.getSize _ expects() returning 20
    mocked.getName _ expects() returning "some.txt"
    mocked.getHeader _ expects * returning null anyNumberOfTimes()
    mocked.getContentType _ expects () returning null
    val fileItems: BufferedIterator[FileItem] = Iterator(FileItem(mocked)).buffered

    fileItems.nextAsZipIfOnlyOne shouldBe None
    fileItems.hasNext shouldBe true
  }
}
