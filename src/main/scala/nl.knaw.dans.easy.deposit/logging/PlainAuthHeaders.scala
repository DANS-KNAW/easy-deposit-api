package nl.knaw.dans.easy.deposit.logging

import javax.servlet.http.HttpServletResponse

trait PlainAuthHeaders extends ResponseLogFormatter {
  override protected def maskAuthHeaders(response: HttpServletResponse): Array[String] = {
    response.getHeaderNames.toArray().map {
      case name: String => s"$name -> ${response.getHeader(name)}"
    }
  }
}
