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
package nl.knaw.dans.easy.validatebag.rules.profile

import nl.knaw.dans.easy.validatebag._
import nl.knaw.dans.easy.validatebag.rules.{ ProfileVersion0, ProfileVersion1 }

class NumberedRulesSpec extends TestSupportFixture with CanConnectFixture {

   private val xmlValidators: Map[String, XmlValidator] = Map(
    "dataset.xml" -> new XmlValidator(null),
    "files.xml" -> new XmlValidator(null),
    "agreements.xml" -> new XmlValidator(null)
  )

  private val allRules: Map[ProfileVersion, RuleBase] = {
    Map(
      0 -> ProfileVersion0(xmlValidators, null),
      1 -> ProfileVersion1(xmlValidators))
  }

  "rulesCheck" should "succeed if all rules, that other rules depend on, exist" in {
    List(0, 1).map(version => {
      val ruleNumbers = allRules(version).map(_.nr)
      val result = allRules(version).flatMap(_.dependsOn).forall(ruleNumber => ruleNumbers.contains(ruleNumber))
      result shouldBe true
    })
  }

  it should "succeed if there are no duplicate rule numbers" in {
    List(0, 1).map(version => {
      val ruleNumbers = allRules(version).map(_.nr)
      ruleNumbers shouldEqual ruleNumbers.distinct
    })
  }
}