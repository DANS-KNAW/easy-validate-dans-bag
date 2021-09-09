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
package nl.knaw.dans.easy.validatebag.rules

import better.files.File.apply
import nl.knaw.dans.easy.validatebag.TargetBag
import nl.knaw.dans.easy.validatebag.validation.fail
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.collection.Set
import scala.util.Try

package object structural extends DebugEnhancedLogging {
  def containsDir(d: Path)(t: TargetBag): Try[Unit] = Try {
    trace(d)
    assume(!d.isAbsolute, s"Directory $d must be a relative path")
    if (!(t.bagDir / d.toString).isDirectory)
      fail(s"Mandatory directory '$d' not found in bag.")
  }

  def doesNotContainFile(f: Path)(t: TargetBag): Try[Unit] = Try {
    trace(f)
    assume(!f.isAbsolute, s"File $f must be a relative path")
    if ((t.bagDir / f.toString).exists)
      fail(s"File '$f' MUST NOT exist in bag (of this information package type).")
  }

  def containsNothingElseThan(d: Path, ps: Seq[String])(t: TargetBag): Try[Unit] = Try {
    trace(d, ps)
    assume(!d.isAbsolute, s"Directory $d must be a relative path")
    val extraFiles = (t.bagDir / d.toString).list.filterNot(ps contains _.name).map(t.bagDir relativize _.path)
    if (extraFiles.nonEmpty) fail(s"Directory $d contains files or directories that are not allowed: ${ extraFiles.mkString(", ") }")
  }

  def hasOnlyValidFileNames(t: TargetBag): Try[Unit] = Try {
    trace(())
    val filesInManifest = t.tryBag.map { bag =>
      bag.getPayLoadManifests.asScala.headOption.getOrElse(fail(s"Dependent rule should have failed: no manifest found for ${ t.bagDir }"))
    }.getOrElse(fail(s"Dependent rule should have failed: Could not get bag ${ t.bagDir }"))
      .getFileToChecksumMap.keySet().asScala.toArray[Path]
    trace(filesInManifest.mkString(", "))

    val invalidCharacters = """:*?"<>|;#"""
    val invalidFiles = filesInManifest.filter { path =>
      invalidCharacters.exists(c => path.name.contains(c))
    }
    trace(invalidFiles.mkString(", "))
    trace(invalidFiles.nonEmpty)
    if (invalidFiles.nonEmpty)
      fail(invalidFiles.mkString("Payload files must have valid characters. Invalid ones: ", ", ", ""))
  }

  val originalFilepathsFile = "original-filepaths.txt"

  def rootContainsOriginalFilepathsFile(t: TargetBag): Boolean = {
    (t.bagDir / originalFilepathsFile).exists
  }

  def readPhysicalToOriginalBagRelativePaths(t: TargetBag): Map[String, String] = {
    val fileToCheck = t.bagDir / originalFilepathsFile
    fileToCheck.lines.map { line =>
      val list = line.split("""[ \t]+""", 2)
      if (list.size != 2) throw new Exception(s"invalid line in $originalFilepathsFile : $line")
      (list(0), list(1))
    }.toMap
  }

  def isOriginalFilepathsFileComplete(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map { xml =>
      val files = xml \ "file"
      val pathsInFilesXmlList = files.map(_ \@ "filepath")
      val pathsInFileXml = pathsInFilesXmlList.toSet
      val filesInBagPayload = (t.bagDir / "data").walk().filter(_.isRegularFile).toSet
      val physicalToOriginalBagRelativePaths = readPhysicalToOriginalBagRelativePaths(t)
      val payloadPaths = filesInBagPayload.map(t.bagDir.path relativize _).map(_.toString)
      val originalFileSetsEqual = pathsInFileXml == physicalToOriginalBagRelativePaths.values.toSet
      val physicalFileSetsEqual = payloadPaths == physicalToOriginalBagRelativePaths.keySet

      if (originalFileSetsEqual && physicalFileSetsEqual) ()
      else {
        def stringDiff[T](name: String, left: Set[T], right: Set[T]): String = {
          val set = left diff right
          if (set.isEmpty) ""
          else s"only in $name: " + set.mkString("{", ", ", "}")
        }

        lazy val onlyInBag = stringDiff("payload", payloadPaths, physicalToOriginalBagRelativePaths.keySet)
        lazy val onlyInFilesXml = stringDiff("files.xml", pathsInFileXml, physicalToOriginalBagRelativePaths.values.toSet)
        lazy val onlyInFilepathsPhysical = stringDiff("physical-bag-relative-path", physicalToOriginalBagRelativePaths.keySet, payloadPaths)
        lazy val onlyInFilepathsOriginal = stringDiff("original-bag-relative-path", physicalToOriginalBagRelativePaths.values.toSet, pathsInFileXml)

        val msg1 = if (physicalFileSetsEqual) ""
                   else s"   - Physical file paths in $originalFilepathsFile not equal to payload in data dir. Difference - " +
                     s"$onlyInBag $onlyInFilepathsPhysical"
        val msg2 = if (originalFileSetsEqual) ""
                   else s"   - Original file paths in $originalFilepathsFile not equal to filepaths in files.xml. Difference - " +
                     s"$onlyInFilepathsOriginal $onlyInFilesXml"

        val msg = msg1 + msg2
        fail(s"$originalFilepathsFile errors:\n$msg")
      }
    }
  }
}
