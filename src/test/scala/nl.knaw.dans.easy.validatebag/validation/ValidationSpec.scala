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
package nl.knaw.dans.easy.validatebag.validation

import nl.knaw.dans.easy.validatebag.{ NumberedRule, RuleNumber, TargetBag, TestSupportFixture }
import nl.knaw.dans.easy.validatebag.InfoPackageType._

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.{ Failure, Success, Try }

class ValidationSpec extends TestSupportFixture {
  private val dummy = bagsDir / "generic-minimal" // Not actually used, but must exist
  private val calls = ListBuffer[String]()

  override def beforeEach() {
    calls.clear()
  }

  private def registerCall(s: String)(b: TargetBag): Try[Unit] = Try {
    calls.append(s)
  }

  private def failingRule(s: String)(b: TargetBag): Try[Unit] = {
    calls.append(s)
    Failure(RuleViolationDetailsException(s"Rule $s failed"))
  }

  private def addNumberedRule(s: String, infoPackageType: InfoPackageType = BOTH, dependsOn: Option[RuleNumber] = None, failing: Boolean = false): NumberedRule = {
    NumberedRule(s, if (failing) failingRule(s)
                    else registerCall(s), infoPackageType, dependsOn)
  }

  "checkRules" should "run all rules in sequence if not filtered" in {
    val rulesBase = Seq(
      addNumberedRule("1"),
      addNumberedRule("2"))
    val result = checkRules(new TargetBag(dummy), rulesBase)(isReadable = _ => true)
    result shouldBe a[Success[_]]
    calls.toList shouldBe List("1", "2")
  }

  it should "leave out AIP-only rules if infoPackageType is SIP" in {
    val rulesBase = Seq(
      addNumberedRule("0", infoPackageType = AIP),
      addNumberedRule("1", infoPackageType = SIP),
      addNumberedRule("2"),
      addNumberedRule("3", infoPackageType = AIP))
    val result = checkRules(new TargetBag(dummy), rulesBase)(isReadable = _ => true)
    result shouldBe a[Success[_]]
    calls.toList shouldBe List("1", "2")
  }

  it should "fail if one rule fails" in {
    val rulesBase = Seq(
      addNumberedRule("1", failing = true),
      addNumberedRule("2"),
      addNumberedRule("3"))
    val result = checkRules(new TargetBag(dummy), rulesBase)(isReadable = _ => true)
    result shouldBe a[Failure[_]]
    calls.toList shouldBe List("1", "2", "3")
  }

  it should "not execute rules that depend (indirectly) on a failing rule" in {
    val rulesBase = Seq(
      addNumberedRule("1", failing = true),
      addNumberedRule("2", dependsOn = Some("1")),
      addNumberedRule("3"),
      addNumberedRule("4", dependsOn = Some("2")),
      addNumberedRule("5", dependsOn = Some("1")))

    val result = checkRules(new TargetBag(dummy), rulesBase)(isReadable = _ => true)
    result shouldBe a[Failure[_]]
    calls.toList shouldBe List("1", "3")
  }
}
