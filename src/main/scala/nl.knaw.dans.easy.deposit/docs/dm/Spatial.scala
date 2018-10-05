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
  require(Try(x.toDouble).isSuccess, s"not a number: x=$x")
  require(Try(y.toDouble).isSuccess, s"not a number: y=$y")

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
  require(Try(north.toDouble).isSuccess, s"Not a number: north=$north")
  require(Try(east.toDouble).isSuccess, s"Not a number: east=$east")
  require(Try(south.toDouble).isSuccess, s"Not a number: south=$south")
  require(Try(west.toDouble).isSuccess, s"Not a number: west=$west")

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