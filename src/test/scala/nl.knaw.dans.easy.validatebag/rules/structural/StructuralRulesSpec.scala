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
package nl.knaw.dans.easy.validatebag.rules.structural

import java.nio.file.Paths

import nl.knaw.dans.easy.validatebag.{ TargetBag, TestSupportFixture }

import scala.util.Failure

class StructuralRulesSpec extends TestSupportFixture {

  "containsDir" should "fail metadata directory not found" in {
    testRuleViolation(containsDir(Paths.get("metadata")), "generic-minimal", "not found in bag")
  }

  it should "fail if target is a file instead of a directory" in {
    testRuleViolation(containsDir(Paths.get("bagit.txt")), "generic-minimal", "not found in bag")
  }

  it should "succeed if directory exists" in {
    testRuleSuccess(containsDir(Paths.get("metadata")), "metadata-correct")
  }

  it should "fail if given an absolute path" in {
    val absolutePath = "/an/absolute/path.jpeg"
    containsDir(Paths.get(absolutePath))(new TargetBag(bagsDir / "generic-minimal-with-binary-data", 0)) should matchPattern {
      case Failure(ae: AssertionError) if ae.getMessage == s"assumption failed: Directory $absolutePath must be a relative path" =>
    }
  }

  "containsNothingElseThan" should "fail if other file is present" in {
    testRuleViolation(
      rule = containsNothingElseThan(Paths.get("metadata"), Seq("dataset.xml", "files.xml")),
      inputBag = "metadata-extra-file",
      includedInErrorMsg = "contains files or directories that are not allowed"
    )
  }

  it should "fail if given an absolute path" in {
    val absolutePath = "/an/absolute/path.jpeg"
    containsNothingElseThan(Paths.get(absolutePath), Seq("dataset.xml", "files.xml"))(new TargetBag(bagsDir / "generic-minimal-with-binary-data", 0)) should matchPattern {
      case Failure(ae: AssertionError) if ae.getMessage == s"assumption failed: Directory $absolutePath must be a relative path" =>
    }
  }

  it should "fail if other directory is present" in {
    testRuleViolation(
      rule = containsNothingElseThan(Paths.get("metadata"), Seq("dataset.xml", "files.xml")),
      inputBag = "metadata-extra-subdir",
      includedInErrorMsg = "contains files or directories that are not allowed"
    )
  }

  it should "succeed less than specified is present" in {
    testRuleSuccess(
      rule = containsNothingElseThan(Paths.get("metadata"), Seq("dataset.xml", "files.xml")),
      inputBag = "metadata-no-files-xml"
    )
  }

  it should "succeed exactly the files/directories specified are present" in {
    testRuleSuccess(
      rule = containsNothingElseThan(Paths.get("metadata"), Seq("dataset.xml", "files.xml")),
      inputBag = "metadata-correct"
    )
  }
}



