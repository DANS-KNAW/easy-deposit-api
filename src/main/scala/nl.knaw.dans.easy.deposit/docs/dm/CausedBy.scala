package nl.knaw.dans.easy.deposit.docs.dm

object CausedBy {
  def unapply(e: Throwable): Option[Throwable] = Option(e.getCause)
}