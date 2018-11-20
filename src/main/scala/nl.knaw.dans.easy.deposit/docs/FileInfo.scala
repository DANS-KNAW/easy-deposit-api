package nl.knaw.dans.easy.deposit.docs

import java.nio.file.Path

/**
 * Information about a file in the deposit
 *
 * @param filename the simple filename of the file
 * @param dirpath  path of the containing directory, relative to the content base directory
 * @param sha1sum  the SHA-1 checksum of the file data
 */
case class FileInfo(filename: String, dirpath: Path, sha1sum: String)
