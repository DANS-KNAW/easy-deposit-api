/*
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
package nl.knaw.dans.easy.deposit.authentication

import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.LdapContext
import org.scalamock.handlers.{ CallHandler0, CallHandler3 }
import org.scalamock.scalatest.MockFactory

case class LdapMocker() extends MockFactory {

  // TODO why not needed in easy-download?
  class MockedSearchResult extends SearchResult("", "", new BasicAttributes())

  val mockedLdapContext: LdapContext = mock[LdapContext]
  private val mockedLdapSearchResults = mock[NamingEnumeration[SearchResult]]
  private val mockedLdapSearchResult = mock[MockedSearchResult]


  def expectLdapSearch: CallHandler3[String, String, SearchControls, NamingEnumeration[SearchResult]] = {
    (mockedLdapContext.search(_: String, _: String, _: SearchControls)) expects(*, *, *)
  }

  def expectLdapClose: CallHandler0[Unit] = {
    (() => mockedLdapContext.close()) expects()
  }

  def expectLdapAttributes(attributes: BasicAttributes): Unit = {
    expectLdapSearch returning mockedLdapSearchResults
    (() => mockedLdapContext.close()) expects()

    (() => mockedLdapSearchResults.hasMoreElements) expects() returning true
    (() => mockedLdapSearchResults.hasMoreElements) expects() returning false
    (() => mockedLdapSearchResults.nextElement) expects() returning mockedLdapSearchResult
    (() => mockedLdapSearchResult.getAttributes) expects() returning attributes
  }
}
