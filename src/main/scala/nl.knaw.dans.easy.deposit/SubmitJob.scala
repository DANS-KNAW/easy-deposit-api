/**
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
import java.nio.file._
import java.nio.file.attribute.PosixFilePermission.{ GROUP_EXECUTE, GROUP_READ, GROUP_WRITE }
import java.nio.file.attribute.{ GroupPrincipal, PosixFileAttributeView }
import java.util.UUID

import better.files.File
import better.files.File.{ CopyOptions, VisitOptions }
import nl.knaw.dans.bag.ChecksumAlgorithm.ChecksumAlgorithm
import nl.knaw.dans.bag.DansBag
import nl.knaw.dans.easy.deposit.docs.AgreementData
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

class SubmitJob(depositId: UUID,
                serializedDatasetXml: String,
                draftBag: DansBag,
                stageBag: DansBag,
                submitDir: File,
                groupPrincipal: GroupPrincipal,
                agreementData: AgreementData,
                agreementGenerator: AgreementGenerator,
                fileLimit: Int,
                mailer: Mailer,
                stateManager: StateManager,
               ) extends Runnable with DebugEnhancedLogging {

  private lazy val agreementDocName = {
    s"agreement.${
      if (agreementGenerator.acceptHeader.contains("html")) "html"
      else "pdf"
    }"
  }

  override def run(): Unit = {
    logger.info(s"[$depositId] starting the dispatched submit action")
    logger.info(s"[$depositId] copy ${ draftBag.data } to ${ stageBag.data / Paths.get(".").toString }")

    submitDeposit() match {
      case Failure(e) =>
        logger.error(s"[$depositId] error in dispatched submit action ${ this.toString }", e)
        stateManager.setStateFailed(e.getMessage)
          .doIfFailure { case e => logger.error(s"[$depositId] could not set state to FAILED after submission failed", e) }
      case Success(()) =>
        sendEmail
          .doIfSuccess(_ => logger.info(s"[$depositId] finished with dispatched submit action"))
          .doIfFailure { case e => logger.error(s"[$depositId] deposit submitted but could not send confirmation message", e) }
    }
  }

  private def submitDeposit(): Try[Unit] = {
    for {
      _ <- stageBag.addPayloadFile(draftBag.data, Paths.get("."))
      _ = logger.info(s"[$depositId] save changes to stageBag ${ stageBag.baseDir }")
      _ <- stageBag.save()
      _ = logger.info(s"[$depositId] validate stageBag ${ stageBag.baseDir }")
      _ <- isValid(stageBag)
      _ = logger.info(s"[$depositId] compare payload manifests of stageBag and draftBag")
      _ <- samePayloadManifestEntries(stageBag, draftBag)
      stageDepositDir = stageBag.baseDir.parent
      _ = logger.info(s"[$depositId] set unix rights for $stageDepositDir")
      _ <- setRightsRecursively(stageDepositDir)
      _ = logger.info(s"[$depositId] move $stageDepositDir to $submitDir")
      _ = stageDepositDir.moveTo(submitDir)(CopyOptions.atomically)
    } yield ()
  }

  private def sendEmail: Try[Unit] = {
    for {
      agreement <- agreementGenerator.generate(agreementData, depositId)
      attachments = Map(
        agreementDocName -> Mailer.pdfDataSource(agreement),
        "metadata.xml" -> Mailer.xmlDataSource(serializedDatasetXml),
        "files.txt" -> Mailer.txtDataSource(serializeManifest(draftBag, fileLimit)),
      )
      email <- mailer.buildMessage(agreementData, attachments, depositId)
      _ = logger.info(s"[$depositId] send email")
      messageId <- Try { email.sendMimeMessage }
      _ = logger.info(s"[$depositId] sent email $messageId")
    } yield ()
  }

  private def serializeManifest(draftBag: DansBag, fileLimit: Int): String = {
    debug("creating manifest")
    val entries = draftBag.payloadManifests.headOption.map(_._2).getOrElse(Map.empty)
    if (entries.size > fileLimit) "" // all files or none avoids confusion
    else entries.map { case (file, sha) => s"$sha ${ draftBag.data.relativize(file) }" }.mkString("\n")
  }

  private def isValid(stageBag: DansBag): Try[Unit] = stageBag.isValid match {
    case Left(msg) => Failure(new Exception(msg))
    case Right(_) => Success(())
  }

  private def samePayloadManifestEntries(staged: DansBag, draft: DansBag) = {
    staged.payloadManifests.keySet.intersect(draft.payloadManifests.keySet)
      .map { algorithm =>
        val xs = getRelativeSet(staged, algorithm)
        val ys = getRelativeSet(draft, algorithm)
        (xs.diff(ys), ys.diff(xs))
      }
      .find(diffs => diffs._1.nonEmpty || diffs._2.nonEmpty)
      .map(diffs => Failure(new Exception(s"staged and draft bag [${ draft.baseDir.parent }] have different payload manifest elements: $diffs")))
      .getOrElse(Success(()))
  }

  private def getRelativeSet(bag: DansBag, algorithm: ChecksumAlgorithm): Set[(Path, String)] = {
    val baseDir = bag.baseDir
    bag.payloadManifests(algorithm).map {
      case (f: File, c: String) => (baseDir.relativize(f), c)
    }.toSet
  }

  private def setRightsRecursively(file: File): Try[Unit] = {
    resource.managed(Files.walk(file.path, Int.MaxValue, VisitOptions.default: _*))
      .map(_.iterator().asScala.map(setRights).find(_.isFailure).getOrElse(Success(())))
      .tried
      .flatten
  }

  private def setRights(path: Path): Try[Unit] = Try {
    trace(path)
    // EASY-1932, variant of https://github.com/DANS-KNAW/easy-split-multi-deposit/blob/73189001217c2bf31b487eb8356f76ea4e9ffc31/src/main/scala/nl.knaw.dans.easy.multideposit/actions/SetDepositPermissions.scala#L72-L90
    val file = File(path)
    file.addPermission(GROUP_WRITE)
    file.addPermission(GROUP_READ)
    if (file.isDirectory)
      file.addPermission(GROUP_EXECUTE)
    // tried File(path).setGroup(groupPrincipal) but it causes "java.io.IOException: 'owner' parameter can't be a group"
    Files.getFileAttributeView(
      path,
      classOf[PosixFileAttributeView],
      LinkOption.NOFOLLOW_LINKS,
    ).setGroup(groupPrincipal)
  }.recoverWith {
    case e: FileSystemException => throw new IOException(s"Not able to set the group to ${ groupPrincipal.getName }. Probably the current user (${ System.getProperty("user.name") }) is not part of this group.", e)
    case e: IOException => throw new IOException(s"Could not set file permissions or group on $path", e)
    case e: SecurityException => throw new IOException(s"Not enough privileges to set file permissions or group on $path", e)
    case NonFatal(e) => throw new IOException(s"unexpected error occured on $path", e)
  }

  override def toString: String = s"<SubmitWorkerAction[id = $depositId, draftBag = ${ draftBag.baseDir }, stageBag = ${ stageBag.baseDir }, submitDir = $submitDir]>"
}
