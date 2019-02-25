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
package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.PossiblySchemed
import nl.knaw.dans.easy.deposit.docs.JsonUtil.toJson
import nl.knaw.dans.easy.deposit.docs.StringUtils._
import nl.knaw.dans.easy.deposit.docs.dm.DateQualifier.DateQualifier
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

object DateQualifier extends Enumeration {
  type DateQualifier = Value
  val created: DateQualifier = Value("dcterms:created")
  val available: DateQualifier = Value("dcterms:available")
  val date: DateQualifier = Value("dc:date")
  val dateAccepted: DateQualifier = Value("dcterms:dateAccepted")
  val dateCopyrighted: DateQualifier = Value("dcterms:dateCopyrighted")
  val dateSubmitted: DateQualifier = Value("dcterms:dateSubmitted")
  val issued: DateQualifier = Value("dcterms:issued")
  val modified: DateQualifier = Value("dcterms:modified")
  val valid: DateQualifier = Value("dcterms:valid")
}

object DateScheme extends Enumeration {
  type DateScheme = Value
  val W3CDTF: DateScheme = Value("dcterms:W3CDTF")
}

case class Date(
                 override val scheme: Option[String],
                 value: Option[String],
                 qualifier: Option[DateQualifier],
               ) extends PossiblySchemed with Mandatory {
  private[docs] override def hasMandatory: Boolean = qualifier.isDefined && value.isProvided
}

object Date {
  private def dateSubmitted: Date = Date(
    Some(DateScheme.W3CDTF.toString),
    Some(DateTime.now().toString(ISODateTimeFormat.date())),
    Some(DateQualifier.dateSubmitted)
  )

  implicit class DatesExtension(val dates: Option[Seq[Date]]) extends AnyVal {
    /**
     * @return (dateCreated, dateAvailable, plainDates)
     */
    private[docs] def separate = {
      dates.getOrElse(Seq.empty)
        .foldLeft((Option.empty[Date], Option.empty[Date], Seq(dateSubmitted))) {
          // @formatter:off
          case ((_,           _,             _     ),      Date(_, _, Some(q@DateQualifier.dateSubmitted))) => invalidQualifier(q)
          case ((None,        dateAvailable, others), date@Date(_, _, Some(  DateQualifier.created))      ) => (Some(date),  dateAvailable, others)
          case ((Some(dc),    _,             _     ), date@Date(_, _, Some(  DateQualifier.created))      ) => duplicateDates(Seq(dc, date))
          case ((dateCreated, None,          others), date@Date(_, _, Some(  DateQualifier.available))    ) => (dateCreated, Some(date),    others)
          case ((_,           Some(da),      _     ), date@Date(_, _, Some(  DateQualifier.available))    ) => duplicateDates(Seq(da, date))
          case ((dateCreated, dateAvailable, others), date@Date(_, _, _)                                  ) => (dateCreated, dateAvailable, others :+ date)
          // @formatter:on
        }
    }

    private def duplicateDates(dates: Seq[Date]) = {
      throw new IllegalArgumentException(s"requirement failed: At most one allowed; got ${ toJson(dates) }")
    }

    private def invalidQualifier(q: DateQualifier) = {
      throw new IllegalArgumentException(s"requirement failed: No $q allowed; got ${ toJson(dates) }")
    }
  }
}
