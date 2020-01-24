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
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.deposit.docs.AgreementData
import nl.knaw.dans.easy.deposit.docs.StateInfo.State
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

class SubmitJob( // deposit values
                 draftDepositId: UUID,
                 draftBag: DansBag,
                 stagedDepositDir: File,
                 datasetXml: String,
                 filesXml: String,
                 agreementsXml: String,
                 msg4DataManager: Option[String],
                 agreementData: AgreementData,
                 draftDepositStateManager: StateManager,
                 // application configuration values
                 groupPrincipal: GroupPrincipal,
                 depositUiURL: String,
                 submitDir: File,
                 agreementGenerator: AgreementGenerator,
                 mailer: Mailer,
               ) extends Runnable with DebugEnhancedLogging {

  private val propsFileName = "deposit.properties"
  private val depositorInfoDirectoryName = "depositor-info"

  override def run(): Unit = {
    logger.info(s"[$draftDepositId] starting the dispatched submit action")

    submitDeposit() match {
      case Failure(e) =>
        logger.error(s"[$draftDepositId] error in dispatched submit action ${ this.toString }", e)
        draftDepositStateManager.setMailToDansDescription(State.submitted)
          .doIfFailure { case e => logger.error(s"[$draftDepositId] could not set state description after submission failed", e) }
      case Success(()) =>
        sendEmail
          .doIfSuccess(_ => logger.info(s"[$draftDepositId] finished with dispatched submit action"))
          .doIfFailure { case e =>
            // applying setMailToDansDescription (with something like no confirmation message sent)
            // could get overwritten by easy-ingest-flow, or worse: overwrite a change by easy-ingest-flow.
            logger.error(s"[$draftDepositId] deposit submitted but could not send confirmation message", e)
          }
    }
  }

  private def submitDeposit(): Try[Unit] = {
    val fullMsg4DataManager = {
      val secondPart = s"The deposit can be found at $depositUiURL/$draftDepositId"
      msg4DataManager.fold(secondPart)(_ + "\n\n" + secondPart)
    }
    logger.debug(s"[$draftDepositId] Message for the datamanager:\n$fullMsg4DataManager")

    val stagedBagDir = stagedDepositDir / bagDirName
    logger.info(s"[$draftDepositId] Created staged bag in ${ stagedBagDir }")
    for {
      stageBag <- DansV0Bag.empty(stagedBagDir).map(_.withCreated())
      _ = (draftBag.baseDir.parent / propsFileName).copyTo(stagedDepositDir / propsFileName)
      _ = logger.info(s"[$draftDepositId] adding metadata to staged bag")
      _ <- stageBag.addMetadataFile(fullMsg4DataManager, s"$depositorInfoDirectoryName/message-from-depositor.txt")
      _ <- stageBag.addMetadataFile(agreementsXml, s"$depositorInfoDirectoryName/agreements.xml")
      _ <- stageBag.addMetadataFile(datasetXml, "dataset.xml")
      _ <- stageBag.addMetadataFile(filesXml, "files.xml")
      _ = logger.info(s"[$draftDepositId] copy payload to staged bag: ${ draftBag.data } to ${ stageBag.data }")
      _ <- stageBag.addPayloadFile(draftBag.data, Paths.get("."))
      _ = logger.info(s"[$draftDepositId] save (tag)manifests to staged bag")
      _ <- stageBag.save()
      _ = logger.info(s"[$draftDepositId] validate staged bag")
      _ <- isValid(stageBag)
      _ = logger.info(s"[$draftDepositId] compare payload manifests of draft bag and staged bag")
      _ <- samePayloadManifestEntries(stageBag, draftBag)
      _ = logger.info(s"[$draftDepositId] set unix rights for $stagedDepositDir")
      _ <- setRightsRecursively(stagedDepositDir)
      _ = logger.info(s"[$draftDepositId] move $stagedDepositDir to $submitDir")
      _ = stagedDepositDir.moveTo(submitDir)(CopyOptions.atomically)
    } yield ()
  }

  private def sendEmail: Try[Unit] = {
    for {
      agreement <- agreementGenerator.generate(agreementData, draftDepositId)
      attachments = Map(
        Mailer.agreementFileName(agreementGenerator.acceptHeader) -> Some(Mailer.dataSource(agreement, agreementGenerator.acceptHeader)),
        Mailer.datasetXmlAttachmentName -> Some(Mailer.xmlDataSource(datasetXml)),
        Mailer.filesAttachmentName -> serializeManifest(draftBag, mailer.fileLimit).map(Mailer.txtDataSource),
      ).collect { case (key, Some(value)) => key -> value }
      email <- mailer.buildMessage(agreementData, attachments, draftDepositId, msg4DataManager)
      _ = logger.info(s"[$draftDepositId] send email")
      messageId <- Try { email.sendMimeMessage }
      _ = logger.info(s"[$draftDepositId] sent email $messageId")
    } yield ()
  }

  private def serializeManifest(draftBag: DansBag, fileLimit: Int): Option[String] = {
    val entries = draftBag.payloadManifests.headOption.map(_._2).getOrElse(Map.empty)
    if (entries.size > fileLimit) None // all files or none avoids confusion
    else Some(entries.map { case (file, sha) => s"$sha ${ draftBag.data.relativize(file) }" }.mkString("\n"))
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
    case e: ProviderMismatchException =>
    // when tested with new GroupPrincipal() {override def getName: String = "invalidGroupPrincipal" }
      throw new IOException(s"unexpected error occurred on $path", e)
    case NonFatal(e) => throw new IOException(s"unexpected error occurred on $path", e)
  }

  override def toString: String = s"<SubmitWorkerAction[id = $draftDepositId, draftBag = ${ draftBag.baseDir }, submitDir = $submitDir]>"
}
