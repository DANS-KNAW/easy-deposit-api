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
package nl.knaw.dans.easy.deposit.datasetfiles

import java.io.InputStream
import java.nio.file.Path
import java.util.UUID

import scala.util.Try

trait DatasetFilesComponent {

  val datasetFiles: DatasetFiles

  trait DatasetFiles {

    /**
     * Returns the list of [[FileInfo]] objects for the dataset files in this deposit.
     *
     * @param user the user ID
     * @param uuid the deposit ID
     * @return
     */
    def getDepositFiles(user: String, uuid: UUID): Try[Seq[FileInfo]] = ???

    /**
     * Writes the given input stream to a location in the deposit's content directory. The specified `path`
     * must be relative to the content directory. The function first ensures that the parent directory
     * exists and creates it, if it doesn't.
     *
     * If a new file is created the function returns `true`, otherwise an existing file was overwritten.
     * (We hope the reader will forgive us this minor violation of the Command-Query separation principle.)
     *
     * @param user the user ID
     * @param uuid the deposit ID
     * @param path the path of the file to (over)write, relative to the content base directory
     * @param is   the input stream to write from
     * @return `true` if a new file was created, `false` otherwise
     */
    def writeDepositFile(user: String, uuid: UUID, path: Path, is: => InputStream): Try[Boolean] = ???

    /**
     * Deletes the given regular file or directory. The specified `path` must be relative to the content directory.
     * If the file is a directory, it is deleted including all the files and directories in it.
     *
     * @param user the user ID
     * @param uuid the deposit ID
     * @param path the path of the file to delete, relative to the content base directory
     * @return
     */
    def deleteDepositFile(user: String, uuid: UUID, path: Path): Try[Unit] = ???

  }
}
