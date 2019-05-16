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
package nl.knaw.dans.easy.validatebag.rules

import java.nio.file.Paths

import nl.knaw.dans.easy.validatebag.{ TargetBag, TestSupportFixture }

import scala.util.Failure

class RulesSpec extends TestSupportFixture {

  "containsFile" should "fail if file name is different case" in {
    /*
     * This test will fail for different reasons on case sensitive and case insensitive file systems respectively. Hence the regex with two
     * alternative error messages.
     */
    testRuleViolationRegex(containsFile(Paths.get("Metadata")), "metadata-correct", "(not found in bag|differs in case)".r)
  }

  it should "fail if target is a directory instead of a file" in {
    testRuleViolation(containsFile(Paths.get("data")), "generic-minimal", "not found in bag")
  }

  it should "succeed if file exists" in {
    testRuleSuccess(containsFile(Paths.get("bagit.txt")), "metadata-correct")
  }

  it should "fail if an absolute path in passed" in {
    val absolutePath = "/an/absolute/path.jpeg"
    containsFile(Paths.get(absolutePath))(new TargetBag(bagsDir / "generic-minimal-with-binary-data", 0)) should matchPattern {
      case Failure(ae: AssertionError) if ae.getMessage == s"assumption failed: File $absolutePath must be a relative path" =>
    }
  }
}
