package nl.knaw.dans.easy.deposit.logging

trait CookieMasker {

  /** The default implementation keeps the name of the cookie, masks everything of the value but dots. */
  protected def maskCookieHeader(value: String): String = {
    val cookieName = value.replaceAll("=.*", "")
    val cookieValue = value.replaceAll(".*=", "")
    val maskedCookieValue = cookieValue.replaceAll("[^.]", "*") // replace everything but dots
    s"$cookieName=$maskedCookieValue"
  }
}
