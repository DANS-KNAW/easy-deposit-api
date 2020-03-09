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
package nl.knaw.dans.easy.deposit.docs

import java.nio.file.Path

/**
 * Information about a file in the deposit
 *
 * @param filename the simple filename of the file
 * @param dirpath  path of the containing directory, relative to the content base directory
 * @param sha1sum  the SHA-1 checksum of the file data
 */
case class FileInfo(filename: String, dirpath: Path, sha1sum: String, size: Long)
