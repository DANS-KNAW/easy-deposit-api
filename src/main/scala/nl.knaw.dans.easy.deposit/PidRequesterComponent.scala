package nl.knaw.dans.easy.deposit

import java.net.URI

import nl.knaw.dans.easy.deposit.PidRequesterComponent.PidType.PidType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import scalaj.http.Http

import scala.util.{ Failure, Success, Try }

trait PidRequesterComponent extends DebugEnhancedLogging {

  val pidRequester: PidRequester

  trait PidRequester {
    val pidGeneratorService: URI

    def requestPid(pidType: PidType): Try[String] = Try {
      Http(s"${ pidGeneratorService }create?type=$pidType")
        .timeout(connTimeoutMs = 10000, readTimeoutMs = 50000)
        .postForm
        .asString
    } flatMap {
      case r if r.code == 201 => Success(r.body)
      case r => Failure(new RuntimeException(s"PID Generator failed: code=${ r.code } body=${ r.body }"))
    }
  }
}
object PidRequesterComponent {
  object PidType extends Enumeration {
    type PidType = Value
    val urn, doi = Value
  }
}
