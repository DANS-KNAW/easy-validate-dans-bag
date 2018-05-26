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

class ValidityRulesSpec extends TestSupportFixture {
  /*
   * We largely rely on bagit-java's validation to be correct.
   */
  "bagIsValid" should "fail if bag dir does not exist" in {
    testRuleViolation(bagIsValid, "XXX", "does not exist", doubleCheckBagItValidity = false)
  }

  it should "fail if bagit.txt is missing" in {
    testRuleViolation(bagIsValid, "bagit-missing-bagittxt", "'bagit.txt' is missing", doubleCheckBagItValidity = false)
  }

  it should "fail if there is a payload file that is not listed in any of the manifests" in {
    testRuleViolation(bagIsValid, "bagit-unchecksummed-payload-file", "isn't listed in any manifest", doubleCheckBagItValidity = false)
  }

  it should "fail if the checksum of a tag file is incorrect" in {
    testRuleViolationRegex(bagIsValid, "bagit-incorrect-tagfile-checksum", "SHA-1.*but was computed".r, doubleCheckBagItValidity = false)
  }
}
