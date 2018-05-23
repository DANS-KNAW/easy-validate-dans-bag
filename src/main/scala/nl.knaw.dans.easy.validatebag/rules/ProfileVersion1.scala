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

import nl.knaw.dans.easy.validatebag.{ NumberedRule, XmlValidator }
import nl.knaw.dans.easy.validatebag.rules.bagit._
import nl.knaw.dans.easy.validatebag.rules.structural._
import nl.knaw.dans.easy.validatebag.InfoPackageType.{ AIP, SIP }

object ProfileVersion1 {
  val versionNumber = 1
  val versionUri = "doi:10.17026/dans-zf3-q54n"

  def apply(implicit xmlValidators: Map[String, XmlValidator]): Seq[NumberedRule] = Seq(
    // BAGIT-RELATED

    // Validity
    NumberedRule("1.1.1", bagMustBeValid, SIP),
    NumberedRule("1.1.2", bagMustBeVirtuallyValid, AIP),

    // bag-info.txt
    NumberedRule("1.2.1", bagMustContainFile(Paths.get("bag-info.txt"))),
    NumberedRule("1.2.2", bagInfoTxtMustContainExactlyOne("BagIt-Profile-Version"), dependsOn = Some("1.2.1")),
    NumberedRule("1.2.2", bagInfoTxtElementMustHaveValue("BagIt-Profile-Version", versionNumber.toString), dependsOn = Some("1.2.2")),
    NumberedRule("1.2.3", bagInfoTxtMustContainExactlyOne("BagIt-Profile-URI"), dependsOn = Some("1.2.1")),
    NumberedRule("1.2.3", bagInfoTxtElementMustHaveValue("BagIt-Profile-URI", versionUri), dependsOn = Some("1.2.3")),
    NumberedRule("1.2.4", bagInfoTxtMustContainExactlyOne("Created"), dependsOn = Some("1.2.1")),
    NumberedRule("1.2.4", bagInfoTxtCreatedMustBeIsoDate, dependsOn = Some("1.2.4")),
    NumberedRule("1.2.5", bagInfoTxtMayContainOne("Is-Version-Of"), dependsOn = Some("1.2.1")),
    NumberedRule("1.2.6", bagInfoTxtMustContainExactlyOne("EASY-User-Account"), AIP, dependsOn = Some("1.2.1")),
    NumberedRule("1.2.6", bagInfoTxtMustNotContain("EASY-User-Account"), SIP, dependsOn = Some("1.2.1")),

    // Manifests
    NumberedRule("1.3.1", bagMustContainFile(Paths.get("manifest-sha1.txt"))),
    NumberedRule("1.3.1", bagSha1PayloadManifestMustContainAllPayloadFiles, dependsOn = Some("1.3.1")),
    // 1.3.2 does not state restrictions, so it does not need checking


    // STRUCTURAL
    NumberedRule("2.1", bagMustContainDir(Paths.get("data/content"))),
    NumberedRule("2.2", bagMustContainDir(Paths.get("data/pdi"))),
    NumberedRule("2.3", bagMustContainDir(Paths.get("data/lic")), AIP),

    // data/pdi
    NumberedRule("2.2.1", bagMustContainFile(Paths.get("data/pdi/dataset.xml")), dependsOn = Some("2.2")),
    NumberedRule("2.2.2", bagMustContainFile(Paths.get("data/pdi/files.xml")), dependsOn = Some("2.2")),
    NumberedRule("2.2.3", bagMustContainFile(Paths.get("data/pdi/emd.xml")), AIP, dependsOn = Some("2.2")),
    NumberedRule("2.2.3", bagMustNotContainFile(Paths.get("data/pdi/emd.xml")), SIP, dependsOn = Some("2.2")),
    NumberedRule("2.2.4", bagMustContainFile(Paths.get("data/pdi/amd.xml")), AIP, dependsOn = Some("2.2")),
    NumberedRule("2.2.4", bagMustNotContainFile(Paths.get("data/pdi/amd.xml")), SIP, dependsOn = Some("2.2")),
    NumberedRule("2.2.5", bagMustContainFile(Paths.get("data/pdi/prov.xml")), AIP, dependsOn = Some("2.2")),
    NumberedRule("2.2.5", bagMustNotContainFile(Paths.get("data/pdi/prov.xml")), SIP, dependsOn = Some("2.2")),
    NumberedRule("2.2.6", bagDirectoryMustNotContainAnythingElseThan(Paths.get("data/pdi"), Seq("dataset.xml", "files.xml", "emd.xml", "amd.xml", "prov.xml ")), dependsOn = Some("2.2")),

    // METADATA
  )
}
