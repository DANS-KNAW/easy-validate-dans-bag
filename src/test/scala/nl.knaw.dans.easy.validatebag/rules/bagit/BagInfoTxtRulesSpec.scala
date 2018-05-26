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
  "bagInfoContainsAtMostOneOf(\"ELEMENT\")" should "fail if bag-info.txt contains two ELEMENT elements" in {
    testRuleViolation(bagInfoContainsAtMostOneOf("ELEMENT"),
      inputBag = "baginfo-two-elements-of-same-key",
      includedInErrorMsg = "may contain at most one")
  }

  it should "succeed if bag-info.txt contains NO ELEMENT element" in {
    testRuleSuccess(bagInfoContainsAtMostOneOf("ELEMENT"),
      inputBag = "baginfo-element-not-present")
  }

  "bagInfoElementIfExistsHasValue(\"ELEMENT\", \"VALUE\")" should "succeed if ELEMENT exists and has value VALUE" in {
    testRuleSuccess(bagInfoElementIfExistsHasValue(
      element = "ELEMENT",
      value = "VALUE"),
      inputBag = "baginfo-one-element-value-present")
  }

  it should "succeed if ELEMENT does NOT exist (as it is optional)" in {
    testRuleSuccess(bagInfoElementIfExistsHasValue(
      element = "ELEMENT",
      value = "VALUE"),
      inputBag = "generic-minimal")
  }

  "bagInfoContainsExactlyOneOf" should "succeed if exactly one ELEMENT present" in {
    testRuleSuccess(bagInfoContainsExactlyOneOf("ELEMENT"),
      inputBag = "baginfo-one-element-value-present")
  }

  it should "fail no ELEMENT present" in {
    testRuleViolation(bagInfoContainsExactlyOneOf("ELEMENT"),
      inputBag = "baginfo-element-not-present",
      includedInErrorMsg = "must contain exactly one")
  }

  it should "fail if TWO ELEMENTs present" in {
    testRuleViolation(bagInfoContainsExactlyOneOf("ELEMENT"),
      inputBag = "baginfo-two-elements-of-same-key",
      includedInErrorMsg = "must contain exactly one")
  }

  "bagInfoCreatedElementIsIso8601Date" should "fail if 'Created' is lacking time and time zone" in {
    testRuleViolation(bagInfoCreatedElementIsIso8601Date,
      inputBag = "baginfo-missing-time-and-timezone-in-created-element",
      includedInErrorMsg = "not in correct ISO 8601 format")
  }

  it should "fail if incorrect date format" in {
    testRuleViolation(bagInfoCreatedElementIsIso8601Date,
      inputBag = "baginfo-non-iso8601-in-created-element",
      includedInErrorMsg = "not in correct ISO 8601 format")
  }

  it should "fail if no millisecond precision provided" in {
    testRuleViolation(bagInfoCreatedElementIsIso8601Date,
      inputBag = "baginfo-no-millisecond-precision-in-created-element",
      includedInErrorMsg = "not in correct ISO 8601 format")
  }

  "bagInfoDoesNotContain" should "fail if the element is present" in {
    testRuleViolation(bagInfoDoesNotContain("ELEMENT"),
      inputBag = "baginfo-one-element-value-present",
      includedInErrorMsg = "must not contain")
  }

  it should "succeed if the element is not present" in {
    testRuleSuccess(bagInfoDoesNotContain("ELEMENT"),
      inputBag = "generic-minimal")
  }

}
