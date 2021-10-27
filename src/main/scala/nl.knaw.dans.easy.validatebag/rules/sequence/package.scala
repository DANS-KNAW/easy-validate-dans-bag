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
package nl.knaw.dans.easy.validatebag.rules

import java.io.IOException
import java.net.URI
import java.util.UUID
import nl.knaw.dans.easy.validatebag.validation.reject
import nl.knaw.dans.easy.validatebag.{BagStore, TargetBag}
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._

package object sequence extends DebugEnhancedLogging {

  def bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStore: BagStore)(t: TargetBag): Try[Unit] = {
    trace(())
    getBagInfoTxtValue(t, "Is-Version-Of")
      .map(_.map(isVersionOf =>
        for {
          uuid <- getUuidFromIsVersionOfValue(isVersionOf)
          exists <- bagStore.bagExists(uuid).recoverWith {
            case _: IOException => Failure(new IllegalStateException(s"Bag with bag-id $uuid, pointed to by Is-Version-Of field in bag-info.txt, could not be verified, because of an I/O error"))
            case e: IllegalStateException => Failure(new IllegalStateException(s"Bag with bag-id $uuid, pointed to by Is-Version-Of field in bag-info.txt, could not be verified: ${e.getMessage}"))
          }
          _ = if (!exists) reject(s"Bag with bag-id $uuid, pointed to by Is-Version-Of field in bag-info.txt, is not found in bag stores")
        } yield ()
      ))
      .flatMap(_.getOrElse(Success(())))
  }

  def storeSameAsInArchivedBag(bagStore: BagStore)(t: TargetBag): Try[Unit] = {
    trace(())
    if (bagStore.getBagStoreUrl != null) {
      getBagInfoTxtValue(t, "Is-Version-Of")
        .map(_.map(isVersionOf =>
          for {
            uuid <- getUuidFromIsVersionOfValue(isVersionOf)
            exists <- bagStore.bagExistsInThisStore(uuid).recoverWith {
              case _: IOException => Try(reject(s"Bag with bag-id $uuid, pointed to by Is-Version-Of field in bag-info.txt, could not be verified, because of an I/O error"))
            }
            _ = if (!exists) reject(s"Bag with bag-id $uuid, pointed to by Is-Version-Of field in bag-info.txt, is not found in bag store ${ bagStore.getBagStoreUrl }")
          } yield ()
        ))
        .flatMap(_.getOrElse(Success(())))
    }
    else {
      logger.info(s"Deep Validation for the store not performed as there was no bag-store provided.")
      Success(())
    }
  }

  def userSameAsInArchivedBag(bagStore: BagStore)(t: TargetBag): Try[Unit] = {
    trace(())
    val user = getUser(t)
    if (user.nonEmpty) {
      getBagInfoTxtValue(t, "Is-Version-Of")
        .map(_.map(isVersionOf =>
          for {
            uuid <- getUuidFromIsVersionOfValue(isVersionOf)
            referredBagInfoText <- bagStore.getBagInfoText(uuid)
            users = referredBagInfoText.split("\n").find(_.startsWith("EASY-User-Account"))
            referredBagUser <- getReferredBagUser(uuid, users)
            _ = if (user != referredBagUser) reject(s"User $user is different from the user $referredBagUser in bag $isVersionOf, pointed to by Is-Version-Of field in bag-info.txt")
          } yield ()
        ))
        .flatMap(_.getOrElse(Success(())))
    }
    else {
      logger.info(s"Deep Validation for the user not performed as there was no user provided in bag-info.txt.")
      Success(())
    }
  }

  private def getUuidFromIsVersionOfValue(s: String): Try[UUID] = Try {
    val uri = new URI(s)
    failIfNotTrueWithMessage(uri.getScheme == "urn", "Is-Version-Of value must be a URN")
    failIfNotTrueWithMessage(uri.getSchemeSpecificPart.startsWith("uuid:"), "Is-Version-Of URN must be of subtype UUID")
    val Array(_, uuidStr) = uri.getSchemeSpecificPart.split(':')
    uuidStr.toUUID.toTry.getOrRecover(e => reject(e.getMessage))
  }

  private def getUser(t: TargetBag): String = {
    getBagInfoTxtValue(t, "EASY-User-Account").getOrElse(reject(s"Could not read the user account from bag-info.txt in ${ t.bagDir }")).getOrElse("")
  }

  def getReferredBagUser(versionOfId: UUID, userLine: Option[String]): Try[String] = Try {
    userLine.map(s => s.split(":")(1).trim).getOrElse(reject(s"No user found for isVersionOf bag $versionOfId"))
  }

  private def failIfNotTrueWithMessage(bool: Boolean, msg: String): Unit = {
    if (!bool) reject(msg)
  }
}
