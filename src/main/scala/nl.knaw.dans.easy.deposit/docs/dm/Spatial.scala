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

import nl.knaw.dans.easy.deposit.docs.StringUtils._

object Spatial {
  /** coordinate order y, x = latitude (DCX_SPATIAL_Y), longitude (DCX_SPATIAL_X) */
  val DEGREES_SRS_NAME = "http://www.opengis.net/def/crs/EPSG/0/4326"

  /** coordinate order x, y = longitude (DCX_SPATIAL_X), latitude (DCX_SPATIAL_Y) */
  val RD_SRS_NAME = "http://www.opengis.net/def/crs/EPSG/0/28992"
}

trait SchemedSpatial extends Mandatory {
  val scheme: Option[String]

  lazy val srsName: String = {
    scheme match {
      case Some("degrees") => Spatial.DEGREES_SRS_NAME
      case Some("RD") => Spatial.RD_SRS_NAME
      case Some(s) if s.trim.nonEmpty => s
      case _ => null // will suppress the XML attribute
    }
  }

  private[docs] override def hasMandatory: Boolean = scheme.isProvided
}

case class SpatialPoint(override val scheme: Option[String],
                        x: Option[String],
                        y: Option[String],
                       ) extends SchemedSpatial {
  private val sx: String = x.getOrElse("")
  private val sy: String = y.getOrElse("")
  lazy val pos: String = srsName match {
    case Spatial.RD_SRS_NAME => s"$sx $sy"
    case Spatial.DEGREES_SRS_NAME => s"$sy $sx"
    case _ => s"$sy $sx"
  }

  private[docs] override def hasMandatory: Boolean = super.hasMandatory && x.isProvided && y.isProvided
}

case class SpatialBox(override val scheme: Option[String],
                      north: Option[String],
                      east: Option[String],
                      south: Option[String],
                      west: Option[String],
                     ) extends SchemedSpatial {
  private lazy val sWest: String = west.getOrElse("")
  private lazy val sSouth: String = south.getOrElse("")
  private lazy val sNorth: String = north.getOrElse("")
  private lazy val sEast: String = east.getOrElse("")
  private lazy val xy = (s"$sWest $sSouth", s"$sEast $sNorth")
  private lazy val yx = (s"$sSouth $sWest", s"$sNorth $sEast")
  /*
   * Note that Y is along North - South and X is along East - West
   * The lower corner is with the minimal coordinate values and upper corner with the maximal coordinate values
   * If x was increasing from West to East and y was increasing from South to North we would have
   * lower corner (x,y) = (West,South) and upper corner (x,y) = (East,North)
   * as shown in the schematic drawing of the box below.
   * This is the case for the WGS84 and RD coordinate systems, but not per se for any other system!
   *
   *                upper(x,y)=(E,N)
   *       *---N---u
   *       |       |
   *       W       E
   *  ^    |       |
   *  |    l---S---*
   *  |  lower(x,y)=(W,S)
   *  y
   *   x -->
   *
   */
  lazy val (lower: String, upper: String) = srsName match {
    case Spatial.RD_SRS_NAME => xy
    case Spatial.DEGREES_SRS_NAME => yx
    case _ => yx
  }

  private[docs] override def hasMandatory: Boolean = super.hasMandatory && north.isProvided && east.isProvided && south.isProvided && west.isProvided
}
