package nl.knaw.dans.easy.deposit

import java.io.IOException
import java.nio.file.StandardCopyOption
import java.nio.file.spi.FileSystemProvider

import better.files.File
import nl.knaw.dans.lib.error._

import scala.util.Try

object Validation {

  @throws[IOException]("when files can not be moved atomically from src to target")
  def sameMounts(srcProvider: FileSystemProvider, src: File, target: File): Unit = {
    val fileName = "same-mount-check"
    val tempFile = src / fileName
    val movedTempFile = target / fileName
    Try {
      if (tempFile.notExists) tempFile.createFile()
      srcProvider.move(tempFile.path, movedTempFile.path, StandardCopyOption.ATOMIC_MOVE)
      movedTempFile.delete()
    }.doIfFailure { case _ => tempFile.delete() }
      .unsafeGetOrThrow
  }
}
