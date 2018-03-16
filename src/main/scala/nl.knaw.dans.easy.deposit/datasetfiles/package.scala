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

import java.nio.file.Path

/**
 * Services for manipulating data files and folders in a deposit. By data files are meant the content
 * files that the user uploads. The deposits into which the files are uploaded must already exist.
 */
package object datasetfiles {

  /**
   * Information about a file in the deposit
   *
   * @param fileName the simple filename of the file
   * @param dirPath path of the containing directory, relative to the content base directory
   * @param sha1sum the SHA-1 checksum of the file data
   */
  case class FileInfo(fileName: String, dirPath: Path, sha1sum: String)
}
