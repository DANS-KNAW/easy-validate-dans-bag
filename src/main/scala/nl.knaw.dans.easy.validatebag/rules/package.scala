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
package nl.knaw.dans.easy.validatebag

import java.nio.file.Path

import nl.knaw.dans.easy.validatebag.validation.fail
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

package object rules extends DebugEnhancedLogging {
  // Relies on there being only one element with the specified name
  def getBagInfoTxtValue(t: TargetBag, element: String): Try[Option[String]] = {
    trace(t, element)
    t.tryBag.map { bag =>
      Option(bag.getMetadata.get(element))
        .flatMap {
          case list if list.isEmpty => None
          case list => Some(list.get(0))
        }
    }
  }

  def containsFile(f: Path)(t: TargetBag) = Try {
    trace(f)
    assume(!f.isAbsolute, s"File $f must be a relative path")
    val fileToCheck = t.bagDir / f.toString
    if (!fileToCheck.isRegularFile)
      fail(s"Mandatory file '$f' not found in bag.")
    val relativeRealPath = t.bagDir.path.relativize(fileToCheck.path.toRealPath())
    val relativeRequiredPath = t.bagDir.path.relativize(fileToCheck.path)
    if (relativeRealPath != relativeRequiredPath)
      fail(s"Path name differs in case; found: $relativeRealPath, required: $relativeRequiredPath")
  }
}
