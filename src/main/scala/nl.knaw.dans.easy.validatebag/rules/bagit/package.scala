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

import java.nio.file.NoSuchFileException

import gov.loc.repository.bagit.domain.Bag
import gov.loc.repository.bagit.exceptions._
import gov.loc.repository.bagit.hash.StandardSupportedAlgorithms._
import gov.loc.repository.bagit.verify.BagVerifier
import nl.knaw.dans.easy.validatebag.TargetBag
import nl.knaw.dans.easy.validatebag.validation._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }

/**
 * Rules that refer back to the BagIt specifications.
 */
package object bagit extends DebugEnhancedLogging {
  private val bagVerifier = new BagVerifier()

  def closeVerifier(): Unit = {
    bagVerifier.close()
  }

  def bagIsValid(t: TargetBag): Try[Unit] = {
    trace(())

    def failBecauseInvalid(t: Throwable): Try[Unit] = {
      val details = s"Bag is not valid: Exception = ${ t.getClass.getSimpleName }, cause = ${ t.getCause }, message = ${ t.getMessage }"
      debug(details)
      Try(fail(details))
    }

    if (!t.bagDir.exists) {
      Try(fail(s"Bag directory does not exist: ${ t.bagDir }"))
    }
    else {
      t.tryBag
        .recoverWith {
          case cause: NoSuchFileException if cause.getMessage.endsWith("bagit.txt") =>
            /*
             * This seems to be the only reason when failing to read the bag should be construed as its being non-valid.
             */
            Try(fail("Mandatory file 'bagit.txt' is missing.")).asInstanceOf[Try[Bag]]
        }
        .map(bagVerifier.isValid(_, false))
        .recoverWith {
          /*
           * Any of these (unfortunately unrelated) exception types mean that the bag is non-valid. The reason is captured in the
           * exception. Any other (non-fatal) exception type means the verification process itself failed;
           * this should lead to a Failure. (Btw fatal errors will NOT be wrapped in a Failure by above Try block!)
           *
           * Note that VerificationException is not included below, as it indicates a error during validation rather
           * than that the bag is non-valid.
           */
          case cause: MissingPayloadManifestException => failBecauseInvalid(cause)
          case cause: MissingBagitFileException => failBecauseInvalid(cause)
          case cause: MissingPayloadDirectoryException => failBecauseInvalid(cause)
          case cause: FileNotInPayloadDirectoryException => failBecauseInvalid(cause)
          case cause: FileNotInManifestException => failBecauseInvalid(cause)
          case cause: MaliciousPathException => failBecauseInvalid(cause)
          case cause: CorruptChecksumException => failBecauseInvalid(cause)
          case cause: UnsupportedAlgorithmException => failBecauseInvalid(cause)
          case cause: InvalidBagitFileFormatException => failBecauseInvalid(cause)
        }
    }
  }

  def bagIsVirtuallyValid(t: TargetBag): Try[Unit] = {
    trace(())
    bagIsValid(t)
      .recoverWith {
        case cause: RuleViolationDetailsException =>
          Try(fail(s"${ cause.details } (WARNING: bag may still be virtually-valid, but this version of the service cannot check that."))
      }
  }

  def bagInfoContainsAtMostOneOf(element: String)(t: TargetBag): Try[Unit] = {
    trace(element)
    t.tryBag.map { bag =>
      Option(bag.getMetadata.get(element))
        .withFilter(_.size() > 1)
        .foreach(_ => fail(s"bag-info.txt may contain at most one element: $element"))
    }
  }

  def bagInfoContainsExactlyOneOf(element: String)(t: TargetBag): Try[Unit] = {
    trace(element)
    t.tryBag.map { bag =>
      val values = bag.getMetadata.get(element)
      val numberFound = Option(values).map(_.size).getOrElse(0)
      if (numberFound != 1) fail(s"bag-info.txt must contain exactly one '$element' element; number found: $numberFound")
    }
  }

  def bagInfoDoesNotContain(element: String)(t: TargetBag): Try[Unit] = {
    trace(element)
    t.tryBag.map { bag =>
      if (bag.getMetadata.contains(element)) fail(s"bag-info.txt must not contain element: $element")
    }
  }

  def bagInfoElementIfExistsHasValue(element: String, value: String)(t: TargetBag): Try[Unit] = {
    trace(element, value)
    getBagInfoTxtValue(t, element).map {
      _.foreach {
        s =>
          if (s != value) fail(s"$element must be $value; found: $s")
      }
    }
  }

  // Relies on there being only one element with the specified name
  private def getBagInfoTxtValue(t: TargetBag, element: String): Try[Option[String]] = {
    trace(t, element)
    t.tryBag.map { bag =>
      Option(bag.getMetadata.get(element))
        .flatMap {
          case list if list.isEmpty => None
          case list => Some(list.get(0))
        }
    }
  }

  def bagInfoCreatedElementIsIso8601Date(t: TargetBag): Try[Unit] = {
    trace(())
    val result = for {
      bag <- t.tryBag
      valueOfCreated <- Option(bag.getMetadata.get("Created"))
        .map {
          case list if list.isEmpty => Failure(RuleViolationDetailsException("No value found for Created-entry in bag-info.txt"))
          case list => Success(list.get(0))
        }
        .getOrElse(Failure(RuleViolationDetailsException("No Created-entry found in bag-info.txt")))
      _ = DateTime.parse(valueOfCreated, ISODateTimeFormat.dateTime)
    } yield ()

    result.map(_ => ()).recoverWith {
      case e: RuleViolationDetailsException => Failure(e)
      case _: Throwable =>
        Failure(RuleViolationDetailsException("Created-entry in bag-info.txt not in correct ISO 8601 format"))
    }
  }

  def bagShaPayloadManifestContainsAllPayloadFiles(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryBag.map { bag =>
      bag.getPayLoadManifests.asScala.find(_.getAlgorithm == SHA1)
        .foreach {
          manifest =>
            val filesInManifest = manifest.getFileToChecksumMap.keySet().asScala.map(p => t.bagDir.relativize(p)).toSet
            debug(s"Manifest files: ${ filesInManifest.mkString(", ") }")
            val filesInPayload = (t.bagDir / "data").walk().filter(_.isRegularFile).map(f => t.bagDir.path.relativize(f.path)).toSet
            debug(s"Payload files: ${ filesInManifest.mkString(", ") }")
            if (filesInManifest != filesInPayload) {
              val filesOnlyInPayload = filesInPayload -- filesInManifest // The other way around should have been caught by the validity check
              fail(s"All payload files must have an SHA-1 checksum. Files missing from SHA-1 manifest: ${ filesOnlyInPayload.mkString(", ") }")
            }
        }
    }
  }
}
