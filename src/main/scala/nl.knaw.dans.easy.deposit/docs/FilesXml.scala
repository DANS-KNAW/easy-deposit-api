package nl.knaw.dans.easy.deposit.docs

import java.nio.file.Path

import better.files.File
import org.apache.tika.Tika

import scala.util.Try
import scala.xml.Elem

object FilesXml {

  /**
   * Generates files.xml from files in draft bag
   *
   * @return Elem
   */
  def apply(pathData: File): Try[Elem] = Try {

    val matches: List[File] = pathData.walk()
      .filter(file => file.isRegularFile)
      .toList

    val tika = new Tika

    <files xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://purl.org/dc/terms/ http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd http://easy.dans.knaw.nl/schemas/bag/metadata/files/ http://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd">
      { matches.map(file =>
      <file filepath={file.toString()}>
        <dcterms:format>{tika.detect(file.toString())}</dcterms:format>
      </file>)
      }
    </files>
  }
}
