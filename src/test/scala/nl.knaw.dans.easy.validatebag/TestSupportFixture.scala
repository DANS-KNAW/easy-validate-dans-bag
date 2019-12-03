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

import java.nio.file.Paths

import better.files._
import nl.knaw.dans.easy.validatebag.rules.bagit.bagIsValid
import nl.knaw.dans.easy.validatebag.validation.RuleViolationDetailsException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest._

import scala.util.matching.Regex
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, NodeSeq }

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfterEach with DebugEnhancedLogging {
  lazy val testDir: File = File(s"target/test/${ getClass.getSimpleName }")
  val ddmSchemaUrl = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  val filesSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/files/2018/04/files.xsd"
  val metadataSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/agreements/2018/12/agreements.xsd"

  protected val bagsDir: File = Paths.get("src/test/resources/bags")

  implicit val isReadable: File => Boolean = _.isReadable

  private def shouldBeValidAccordingToBagIt(inputBag: String): Unit = {
    bagIsValid(new TargetBag(bagsDir / inputBag, 0)) shouldBe a[Success[_]] // Profile version does not matter here
  }

  protected def testRuleViolationRegex(rule: Rule, inputBag: String, includedInErrorMsg: Regex, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    val result = rule(new TargetBag(bagsDir / inputBag, profileVersion))
    if (doubleCheckBagItValidity) shouldBeValidAccordingToBagIt(inputBag)
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e: RuleViolationDetailsException) =>
        e.getMessage should include regex includedInErrorMsg
    }
  }

  protected def testRuleViolation(rule: Rule, inputBag: String, includedInErrorMsg: String, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    val result = rule(new TargetBag(bagsDir / inputBag, profileVersion))
    if (doubleCheckBagItValidity) shouldBeValidAccordingToBagIt(inputBag)
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e: RuleViolationDetailsException) =>
        e.getMessage should include(includedInErrorMsg)
      case Failure(e) => fail(s"Expecting [$includedInErrorMsg] got: $e")
    }
  }

  protected def testRuleSuccess(rule: Rule, inputBag: String, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    if (doubleCheckBagItValidity) shouldBeValidAccordingToBagIt(inputBag)
    rule(new TargetBag(bagsDir / inputBag, profileVersion)) shouldBe a[Success[_]]
  }

  protected def ruleFailure(message: String): Failure[RuleViolationDetailsException] = {
    Failure(RuleViolationDetailsException(message))
  }

  protected def bagWithExtraDcmi(extras: NodeSeq): TargetBag = {
    // TODO for a new pull request: apply to more tests,
    //  it reduces test resources and shows the essentials in one view
    new TargetBag(bagsDir / "metadata-correct", profileVersion = 0) {
      override lazy val tryDdm: Try[Node] = Success(
        <ddm:DDM xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
                   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                   xmlns:dc="http://purl.org/dc/elements/1.1/"
                   xmlns:dct="http://purl.org/dc/terms/"
                   xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
                   xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2017/09/ddm.xsd">
              <ddm:profile>
                  <dc:title xml:lang="en">Title of the dataset</dc:title>
                  <dc:description xml:lang="la">Lorem ipsum dolor sit amet, consectetur adipiscing elit.</dc:description>
                  <dcx-dai:creatorDetails>
                      <dcx-dai:author>
                          <dcx-dai:organization>
                              <dcx-dai:name xml:lang="en">Utrecht University</dcx-dai:name>
                          </dcx-dai:organization>
                      </dcx-dai:author>
                  </dcx-dai:creatorDetails>
                  <ddm:created>2012-12</ddm:created>
                  <ddm:available>2013-05</ddm:available>
                  <ddm:audience>D24000</ddm:audience>
                  <ddm:accessRights>OPEN_ACCESS_FOR_REGISTERED_USERS</ddm:accessRights>
              </ddm:profile>
              <ddm:dcmiMetadata>
                  <dct:license xsi:type="dct:URI">http://creativecommons.org/licenses/by-sa/4.0</dct:license>
                  <dct:rightsHolder>Mr. Rights</dct:rightsHolder>
                  { extras }
              </ddm:dcmiMetadata>
          </ddm:DDM>)
    }
  }
}
