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

class ManifestRulesSpec extends TestSupportFixture {
  "bagShaPayloadManifestContainsAllPayloadFiles" should "fail if not all payload files have a SHA-1 checksum" in {
    testRuleViolationRegex(
      bagShaPayloadManifestContainsAllPayloadFiles,
      inputBag = "bagit-two-payload-files-without-sha1",
      includedInErrorMsg = """All payload files must have an SHA-1 checksum.*sine-sha1.txt""".r
    )
  }

  it should "succeed if not all payload files have an MD5 checksum" in {
    testRuleSuccess(
      bagShaPayloadManifestContainsAllPayloadFiles,
      inputBag = "bagit-two-payload-files-without-md5"
    )
  }
}
