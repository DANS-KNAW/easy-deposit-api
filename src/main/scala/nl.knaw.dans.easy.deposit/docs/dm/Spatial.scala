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

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.RequiresNonEmpty

import scala.util.Try

object Spatial {
  /** coordinate order y, x = latitude (DCX_SPATIAL_Y), longitude (DCX_SPATIAL_X) */
  val DEGREES_SRS_NAME = "http://www.opengis.net/def/crs/EPSG/0/4326"

  /** coordinate order x, y = longitude (DCX_SPATIAL_X), latitude (DCX_SPATIAL_Y) */
  val RD_SRS_NAME = "http://www.opengis.net/def/crs/EPSG/0/28992"
}

trait SchemedSpatial extends RequiresNonEmpty {
  val scheme: String
  requireNonEmptyString(scheme, "scheme")

  lazy val srsName: String = {
    scheme match {
      case "degrees" => Spatial.DEGREES_SRS_NAME
      case "RD" => Spatial.RD_SRS_NAME
      case s if s.trim.isEmpty => null // will suppress the XML attribute
      case _ => scheme
    }
  }
}

case class SpatialPoint(override val scheme: String,
                        x: String,
                        y: String,
                       ) extends RequiresNonEmpty with SchemedSpatial {
  // using doubles as arguments could change precision in the XML output by adding ".0"
  require(Try(x.toDouble).isSuccess, msg("x"))
  require(Try(y.toDouble).isSuccess, msg("y"))
  def msg(s: String) = s"$s is not a number in {scheme:$scheme, x:$x, y:$y}"

  lazy val pos: String = srsName match {
    case Spatial.RD_SRS_NAME => s"$x $y"
    case Spatial.DEGREES_SRS_NAME => s"$y $x"
    case _ => s"$y $x"
  }
}

case class SpatialBox(override val scheme: String,
                      north: String,
                      east: String,
                      south: String,
                      west: String,
                     ) extends RequiresNonEmpty  with SchemedSpatial {
  // using doubles as arguments could change precision in the XML output by adding ".0"
  require(Try(north.toDouble).isSuccess, msg("north"))
  require(Try(east.toDouble).isSuccess, msg("east"))
  require(Try(south.toDouble).isSuccess, msg("south"))
  require(Try(west.toDouble).isSuccess, msg("west"))
  def msg(s: String) = s"$s is not a number in {scheme:$scheme, north:$north, east:$east, south:$south, wets:$west}"

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
  private lazy val xy = (s"$west $south", s"$east $north")
  private lazy val yx = (s"$south $west", s"$north $east")
  lazy val (lower:String, upper: String) = srsName match {
    case Spatial.RD_SRS_NAME => xy
    case Spatial.DEGREES_SRS_NAME => yx
    case _ => yx
  }
}
