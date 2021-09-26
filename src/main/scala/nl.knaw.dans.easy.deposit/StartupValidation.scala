/*
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.deposit

import java.io.IOException

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.lib.error._

import scala.util.Try

object StartupValidation {

  @throws[IOException]("when files can not be moved atomically from src to target")
  def allowsAtomicMove(srcDir: File, targetDir: File): Unit = {
    val fileName = "same-mount-check"
    val srcFile = srcDir / fileName
    val targetFile = targetDir / fileName
    Try {
      if (srcFile.notExists) srcFile.createFile()
      srcFile.moveTo(targetFile)(CopyOptions.atomically)
      targetFile.delete()
    }.doIfFailure { case _ => srcFile.delete() }
      .unsafeGetOrThrow
  }
}
