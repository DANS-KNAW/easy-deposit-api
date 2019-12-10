package nl.knaw.dans.easy.deposit

import java.io.InputStream
import java.net.URL

import better.files.StringExtensions
import scalaj.http.BaseHttp

import scala.util.{ Success, Try }

trait AgreementGenerator {
  val http: BaseHttp
  val url: URL

  def agreementDoc(): Try[InputStream] = Success("not yet implemented".inputStream)
}
