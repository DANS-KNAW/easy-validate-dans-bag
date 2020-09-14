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

import better.files._
import gov.loc.repository.bagit.reader.BagReader
import nl.knaw.dans.easy.validatebag.InfoPackageType.{ InfoPackageType, _ }
import nl.knaw.dans.easy.validatebag.rules.ProfileVersion0
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

package object validation extends DebugEnhancedLogging {

  private val bagReader = new BagReader()

  /**
   * Exception specifying the rule violated and what the violation consisted of. The number refers back
   * to the DANS BagIt Profile documents.
   *
   * @param ruleNr  rule number violated.
   * @param details details about the violation
   */
  case class RuleViolationException(ruleNr: RuleNumber, details: String) extends Exception(s"[$ruleNr] $details")

  /**
   * Internal exception, only specifying the details of a rule violation, but not the rule number, as this number
   * may vary across versions of the Profile.
   *
   * @param details details about the violation
   */
  case class RuleViolationDetailsException(details: String) extends Exception(details)

  private case class RuleNotApplicableException() extends Exception("rule not applicable, testing for something else")

  /**
   * Signals a rule violation. This function should be called from the rule functions to indicate that
   * the rule was not met.
   *
   * @param details the details about the rule violation
   */
  def  fail[T](details: String): T = throw RuleViolationDetailsException(details)

  /**
   * Validates if the bag pointed to is compliant with the DANS BagIt Profile version it claims to
   * adhere to. If no claim is made, by default it is assumed that the bag is supposed to comply
   * with v0.
   *
   * @param bag               the bag to validate
   * @param ruleBase          a map containing a RuleBase for each profile version
   * @param asInfoPackageType validate as SIP (default) or AIP
   * @param isReadable        function to check the readability of a file (added for unit testing purposes)
   * @return Success if compliant, Failure if not compliant or an error occurred. The Failure will contain
   *         `nl.knaw.dans.lib.error.CompositeException`, which will contain a [[RuleViolationException]]
   *         for every violation of the DANS BagIt Profile rules. If a fatal exception occurred, the Failure will
   *         contain only that exception.
   */
  def checkRules(bag: TargetBag, ruleBase: RuleBase, asInfoPackageType: InfoPackageType = SIP)(implicit isReadable: File => Boolean): Try[Unit] = {
    /**
     * `isReadable` was added because unit testing this by actually setting files on the file system to non-readable and back
     * can get messy. After a failed build one might be left with a target folder that refuses to be cleaned. Unless you are
     * aware of the particular details of the test this will be very confusing.
     */
    trace(bag, asInfoPackageType)
    for {
      _ <- checkIfValidationCanProceed(bag.bagDir)
      result <- evaluateRules(bag, ruleBase, asInfoPackageType)
    } yield result
  }

  private def evaluateRules(bag: TargetBag, ruleBase: RuleBase, asInfoPackageType: InfoPackageType = SIP): Try[Unit] = Try {
    ruleBase.filter(numberedRule => numberedRule.infoPackageType == BOTH || numberedRule.infoPackageType == asInfoPackageType)
      .foldLeft(List[Try[String]]()) {
        (results, numberedRule) =>
          numberedRule match {
            case NumberedRule(nr, rule, _, dependsOn) if dependsOn.forall(results.collect { case Success(succeededRuleNr) => succeededRuleNr }.contains) =>
              val nextResult = rule(bag)
                .map(_ => nr) match {
                case Success(value) => Success(value)
                case Failure(RuleViolationDetailsException(details)) => Failure(RuleViolationException(nr, details))
                case Failure(e) => throw e // failing fast on unexpected type of exception
              }
              results :+ nextResult
            case _ => results
          }
      }.collectResults.map(_ => ())
  }.flatten

  private def checkIfValidationCanProceed(bag: BagDir)(implicit isReadable: File => Boolean): Try[Unit] = Try {
    trace(bag)
    debug(s"Checking existence of $bag")
    require(bag.exists, "Bag does not exist")
    debug(s"Checking readability of $bag")
    require(isReadable(bag), s"Bag is non-readable") // Using the passed-in isReadable function here!!
    debug(s"Checking if $bag is directory")
    require(bag.isDirectory, "Bag must be a directory")
    bag.walk().foreach {
      f =>
        debug(s"Checking readability of $f")
        require(isReadable(f), s"Found non-readable file $f") // Using the passed-in isReadable function here!!
    }
  }

  def getProfileVersion(bag: BagDir): Try[ProfileVersion] = Try {
    if ((bag / "bag-info.txt").exists) {
      Try { bagReader.read(bag.path) } // Reading the bag will fail if bag-info.txt is malformed
        .map { b =>
          Option(b.getMetadata.get("BagIt-Profile-Version")).map(_.asScala.toList).map {
            case v :: _ => Try { v.toInt }.filter(profileVersionDois.keys.toSet.contains).getOrElse(ProfileVersion0.versionNumber)
            case _ => ProfileVersion0.versionNumber
          }.getOrElse(ProfileVersion0.versionNumber)
        }
    }.getOrElse(ProfileVersion0.versionNumber)
    else ProfileVersion0.versionNumber
  }
}
