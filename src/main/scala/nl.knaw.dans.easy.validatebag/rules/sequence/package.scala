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

import java.io.IOException
import java.net.URI
import java.util.UUID

import nl.knaw.dans.easy.validatebag.validation.fail
import nl.knaw.dans.easy.validatebag.{ BagStore, TargetBag }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }

package object sequence extends DebugEnhancedLogging {
  private val uuidHexLength = 32
  private val uuidCanonicalNumberOfDashes = 4

  def bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStore: BagStore)(t: TargetBag): Try[Unit] = {
    trace(())
    getBagInfoTxtValue(t, "Is-Version-Of")
      .map(_.map(isVersionOf =>
        for {
          uuid <- getUuidFromIsVersionOfValue(isVersionOf)
          exists <- bagStore.bagExists(uuid).recoverWith {
            case _: IOException => Try(fail(s"Bag with bag-id $uuid, pointed to by Is-Version-Of field in bag-info.txt, could not be verified, because of an I/O error"))
          }
          _ = if (!exists) fail(s"Bag with bag-id $uuid, pointed to by Is-Version-Of field in bag-info.txt, is not found in bag stores")
        } yield ()
      ))
      .flatMap(_.getOrElse(Success(())))
  }

  private def getUuidFromIsVersionOfValue(s: String): Try[UUID] = Try {
    val uri = new URI(s)
    failIfNotTrueWithMessage(uri.getScheme == "urn", "Is-Version-Of value must be a URN")
    failIfNotTrueWithMessage(uri.getSchemeSpecificPart.startsWith("uuid:"), "Is-Version-Of URN must be of subtype UUID")
    val Array(_, uuidStr) = uri.getSchemeSpecificPart.split(':')
    failIfNotTrueWithMessage(uuidStr.length == uuidHexLength + uuidCanonicalNumberOfDashes, "UUID must be in canonical textual representation")
    UUID.fromString(uuidStr)
  }

  private def failIfNotTrueWithMessage(bool: Boolean, msg: String): Unit = {
    if (!bool) fail(msg)
  }
}
