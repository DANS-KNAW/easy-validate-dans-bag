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

import javax.xml.validation.{ Schema, SchemaFactory }
import nl.knaw.dans.easy.validatebag._
import nl.knaw.dans.easy.validatebag.rules.{ ProfileVersion0, ProfileVersion1 }
import org.scalatest.Inspectors

class NumberedRulesSpec extends TestSupportFixture with Inspectors {

  private val schemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema")

  private val xmlValidators: Map[String, XmlValidator] = Map(
    "dataset.xml" -> new XmlValidator(schemaFactory.newSchema),
    "files.xml" -> new XmlValidator(schemaFactory.newSchema),
    "agreements.xml" -> new XmlValidator(schemaFactory.newSchema)
  )

  private val allRules: Map[ProfileVersion, RuleBase] = {
    Map(
      0 -> ProfileVersion0(xmlValidators, null, null),
      1 -> ProfileVersion1(xmlValidators))
  }

  "rulesCheck" should "succeed if all rules, that other rules depend on, exist" in {
    forEvery(allRules.values) {
      ruleBase => {
        val ruleNumbers = ruleBase.map(_.nr)
        forEvery(ruleBase.flatMap(_.dependsOn)) {
          dependency => ruleNumbers should contain (dependency)
        }
      }
    }
  }

  forEvery(allRules.values) {
    ruleBase => {
      val ruleNumbers = ruleBase.map(_.nr)
      ruleNumbers shouldEqual ruleNumbers.distinct
    }
  }
}
