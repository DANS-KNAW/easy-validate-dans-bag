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
package nl.knaw.dans.easy.validatebag.rules.bagit

import nl.knaw.dans.easy.validatebag.TestSupportFixture

class BagInfoTxtRulesSpec extends TestSupportFixture {
  "bagInfoTxtMayContainOne(\"ELEMENT\")" should "fail if bag-info.txt contains two ELEMENT elements" in {
    testRuleViolation(bagInfoTxtMayContainOne("ELEMENT"),
      inputBag = "two-many-ELEMENT-in-bag-info.txt",
      includedInErrorMsg = "may contain at most one",
      doubleCheckBagItValidity = true)
  }

  it should "succeed if bag-info.txt contains NO ELEMENT element" in {
    testRuleSuccess(bagInfoTxtMayContainOne("ELEMENT"),
      inputBag = "zero-ELEMENT-in-bag-info",
      doubleCheckBagItValidity = true)
  }

  "bagInfoTxtOptionalElementMustHaveValue(\"ELEMENT\", \"VALUE\")" should "succeed if ELEMENT exists and has value VALUE" in {
    testRuleSuccess(bagInfoTxtElementMustHaveValue(
      element = "ELEMENT",
      value = "VALUE"),
      inputBag = "one-ELEMENT-VALUE-in-bag-info",
      doubleCheckBagItValidity = true)
  }

  // TODO: TEST it should succeed if element does not exist

  // TODO: TEST bagInfoTxtMustContainExactlyOne

  "bagInfoTxtCreatedMustBeIsoDate" should "fail if 'Created' is lacking time and time zone" in {
    testRuleViolation(bagInfoTxtCreatedMustBeIsoDate,
      inputBag = "missing-time-and-timezone-in-Created",
      includedInErrorMsg = "not in correct ISO 8601 format",
      doubleCheckBagItValidity = true)
  }

  it should "fail if incorrect date format" in {
    testRuleViolation(bagInfoTxtCreatedMustBeIsoDate,
      inputBag = "non-ISO8601-in-Created",
      includedInErrorMsg = "not in correct ISO 8601 format",
      doubleCheckBagItValidity = true)
  }

  it should "fail if no millisecond precision provided" in {
    testRuleViolation(bagInfoTxtCreatedMustBeIsoDate,
      inputBag = "no-millisecond-precision-in-Created",
      includedInErrorMsg = "not in correct ISO 8601 format",
      doubleCheckBagItValidity = true)
  }

  "bagInfoTxtMustNotContain" should "fail if the element is present" in {
    testRuleViolation(bagInfoTxtMustNotContain("ELEMENT"),
      inputBag = "one-ELEMENT-VALUE-in-bag-info",
      includedInErrorMsg = "must not contain",
      doubleCheckBagItValidity = true)
  }

  it should "succeed if the element is not present" in {
    testRuleSuccess(bagInfoTxtMustNotContain("ELEMENT"),
      inputBag = "minimal",
      doubleCheckBagItValidity = true)
  }

}
