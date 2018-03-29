package nl.knaw.dans.easy.deposit.components

import java.nio.file.{ Path, Paths }
import java.text.SimpleDateFormat

import nl.knaw.dans.easy.deposit.{ DatasetMetadata, State, StateInfo }
import org.json4s.JsonAST.{ JNull, JString }
import org.json4s.ext.{ EnumNameSerializer, JodaTimeSerializers, UUIDSerializer }
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization.write
import org.json4s.{ CustomSerializer, DefaultFormats, Formats, JsonInput }

import scala.util.{ Failure, Try }

object Json {

  class InvalidDocument(s: String, t: Throwable) extends Exception(s, t)

  class PathSerializer extends CustomSerializer[Path](format =>
    ( {
      case JString(s) => Paths.get(s)
      case JNull => null
    }, {
      case x: Path => JString(x.toString)
    }
    )
  )

  private implicit val jsonFormats: Formats = new DefaultFormats {
    // TODO we need a timestamp for DepositInfo, but may need dates for not yet implemented members of DatasetMetadata
    override protected def dateFormatter: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
  } +
    UUIDSerializer +
    new PathSerializer +
    new EnumNameSerializer(State) ++
    JodaTimeSerializers.all

  def toJson[A <: AnyRef](a: A): String = {
    // seems not to need a try: while the date formatter wasn't in place it produced empty strings
    write(a)
  }

  def getUser(body: JsonInput): Try[User] = Try {
    JsonMethods.parse(body).extract[User]
  }.recoverWith { case t: Throwable => Failure(new InvalidDocument("User", t)) }

  def getStateInfo(body: JsonInput): Try[StateInfo] = Try {
    JsonMethods.parse(body).extract[StateInfo]
  }.recoverWith { case t: Throwable => Failure(new InvalidDocument("StateInfo", t)) }

  def getDatasetMetadata(body: JsonInput): Try[DatasetMetadata] = Try {
    JsonMethods.parse(body).extract[DatasetMetadata]
  }.recoverWith { case t: Throwable => Failure(new InvalidDocument("DatasetMetadata", t)) }
}
