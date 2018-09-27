package nl.knaw.dans.easy.deposit.docs.dm

import nl.knaw.dans.easy.deposit.docs.DatasetMetadata.RequiresNonEmpty

case class SpatialPoint(scheme: String,
                        x: Int,
                        y: Int,
                       ) extends RequiresNonEmpty {
  requireNonEmptyString(scheme, "scheme")
}

case class SpatialBox(scheme: String,
                      north: Int,
                      east: Int,
                      south: Int,
                      west: Int,
                     ) extends RequiresNonEmpty {
  requireNonEmptyString(scheme, "scheme")
}