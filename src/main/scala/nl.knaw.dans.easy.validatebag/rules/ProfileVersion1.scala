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

import nl.knaw.dans.easy.validatebag.InfoPackageType.{ AIP, SIP }
import nl.knaw.dans.easy.validatebag.rules.bagit._
import nl.knaw.dans.easy.validatebag.rules.structural._
import nl.knaw.dans.easy.validatebag.{ NumberedRule, XmlValidator }

object ProfileVersion1 {
  val versionNumber = 1
  val versionUri = "doi:10.17026/dans-zf3-q54n"

  def apply(implicit xmlValidators: Map[String, XmlValidator]): Seq[NumberedRule] = Seq(
    // BAGIT-RELATED

    // Validity
    NumberedRule("1.1.1", bagIsValid, SIP),
    NumberedRule("1.1.2", bagIsVirtuallyValid, AIP),

    // bag-info.txt
    NumberedRule("1.2.1", containsFile(Paths.get("bag-info.txt"))),
    NumberedRule("1.2.2(a)", bagInfoContainsExactlyOneOf("BagIt-Profile-Version"), dependsOn = List("1.2.1")),
    NumberedRule("1.2.2(b)", bagInfoElementIfExistsHasValue("BagIt-Profile-Version", versionNumber.toString), dependsOn = List("1.2.2(a)")),
    NumberedRule("1.2.3(a)", bagInfoContainsExactlyOneOf("BagIt-Profile-URI"), dependsOn = List("1.2.1")),
    NumberedRule("1.2.3(b)", bagInfoElementIfExistsHasValue("BagIt-Profile-URI", versionUri), dependsOn = List("1.2.3(a)")),
    NumberedRule("1.2.4(a)", bagInfoContainsExactlyOneOf("Created"), dependsOn = List("1.2.1")),
    NumberedRule("1.2.4(b)", bagInfoCreatedElementIsIso8601Date, dependsOn = List("1.2.4(a)")),
    NumberedRule("1.2.5", bagInfoContainsAtMostOneOf("Is-Version-Of"), dependsOn = List("1.2.1")),
    NumberedRule("1.2.6(a)", bagInfoContainsExactlyOneOf("EASY-User-Account"), AIP, dependsOn = List("1.2.1")),
    // 1.2.6(b) does not state restrictions, so it does not need checking

    // Manifests
    NumberedRule("1.3.1(a)", containsFile(Paths.get("manifest-sha1.txt")), AIP),
    NumberedRule("1.3.1(b)", bagShaPayloadManifestContainsAllPayloadFiles, AIP, dependsOn = List("1.3.1(a)")),
    // 1.3.2 does not state restrictions, so it does not need checking


    // STRUCTURAL
    NumberedRule("2.1", containsDir(Paths.get("data/content"))),
    NumberedRule("2.2", containsDir(Paths.get("data/pdi"))),
    NumberedRule("2.3", containsDir(Paths.get("data/lic")), AIP),

    // data/pdi
    NumberedRule("2.2.1", containsFile(Paths.get("data/pdi/dataset.xml")), dependsOn = List("2.2")),
    NumberedRule("2.2.2", containsFile(Paths.get("data/pdi/files.xml")), dependsOn = List("2.2")),
    NumberedRule("2.2.3(a)", containsFile(Paths.get("data/pdi/emd.xml")), AIP, dependsOn = List("2.2")),
    NumberedRule("2.2.3(b)", doesNotContainFile(Paths.get("data/pdi/emd.xml")), SIP, dependsOn = List("2.2")),
    NumberedRule("2.2.4(a)", containsFile(Paths.get("data/pdi/amd.xml")), AIP, dependsOn = List("2.2")),
    NumberedRule("2.2.4(b)", doesNotContainFile(Paths.get("data/pdi/amd.xml")), SIP, dependsOn = List("2.2")),
    NumberedRule("2.2.5(a)", containsFile(Paths.get("data/pdi/prov.xml")), AIP, dependsOn = List("2.2")),
    NumberedRule("2.2.5(b)", doesNotContainFile(Paths.get("data/pdi/prov.xml")), SIP, dependsOn = List("2.2")),
    NumberedRule("2.2.6", containsNothingElseThan(Paths.get("data/pdi"), Seq("dataset.xml", "files.xml", "emd.xml", "amd.xml", "prov.xml ")), dependsOn = List("2.2")),

    // METADATA
  )
}
