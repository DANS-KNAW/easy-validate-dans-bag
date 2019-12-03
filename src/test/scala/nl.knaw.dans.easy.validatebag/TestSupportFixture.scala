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
import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml.{ Elem, Node, NodeSeq, Text }

trait TestSupportFixture extends FlatSpec with Matchers with Inside with BeforeAndAfterEach with DebugEnhancedLogging {
  lazy val testDir: File = File(s"target/test/${ getClass.getSimpleName }")
  val ddmSchemaUrl = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  val filesSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/files/2018/04/files.xsd"
  val metadataSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/agreements/2018/12/agreements.xsd"

  protected val bagsDir: File = Paths.get("src/test/resources/bags")

  implicit val isReadable: File => Boolean = _.isReadable

  private def shouldBeValidAccordingToBagIt(inputBag: String): Unit = {
    bagIsValid(bag(inputBag = inputBag)) shouldBe a[Success[_]] // Profile version does not matter here
  }

  protected def testRuleViolationRegex(rule: Rule, inputBag: String, includedInErrorMsg: Regex, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    val result = rule(bag(inputBag = inputBag, profileVersion = profileVersion))
    if (doubleCheckBagItValidity) shouldBeValidAccordingToBagIt(inputBag)
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e: RuleViolationDetailsException) =>
        e.getMessage should include regex includedInErrorMsg
    }
  }

  protected def testRuleViolation(rule: Rule, inputBag: String, includedInErrorMsg: String, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    val result = rule(bag(inputBag = inputBag, profileVersion = profileVersion))
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
    rule(bag(inputBag = inputBag, profileVersion = profileVersion)) shouldBe a[Success[_]]
  }

  protected def ruleFailure(message: String): Failure[RuleViolationDetailsException] = {
    Failure(RuleViolationDetailsException(message))
  }

  protected def bag(extraDcmi: NodeSeq = Text(""), inputBag: String = "metadata-correct", profileVersion: ProfileVersion = 0): TargetBag = {
    // TODO for a new pull request: apply to more tests,
    //  it reduces test resources and shows the essentials in one view
    new TargetBag(bagsDir / inputBag, profileVersion) {
      override lazy val tryDdm: Try[Node] = loadDdm.flatMap(addToDcmiMetadata(_, extraDcmi))
    }
  }

  // copy of FlowStepEnrichMetadataSpec.addToDcmiMetadata (just a rename of the object and Node -> NodeSeq)
  private def addToDcmiMetadata(ddm: Node, additional: NodeSeq): Try[Node] = Try {
    object DcmiRule extends RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case Elem(boundPrefix, "dcmiMetadata", _, boundScope, children @ _*) =>
          <dcmiMetadata>
            {additional}
            {children}
          </dcmiMetadata>.copy(prefix = boundPrefix, scope = boundScope)
        case other => other
      }
    }
    new RuleTransformer(DcmiRule).transform(ddm).head
  }
}
