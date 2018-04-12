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

import java.net.URL
import java.nio.file.Paths

import nl.knaw.dans.easy.validatebag.{ profileVersion0, profileVersion0Uri, NumberedRule, XmlValidator }
import nl.knaw.dans.easy.validatebag.rules.bagit._
import nl.knaw.dans.easy.validatebag.rules.metadata._
import nl.knaw.dans.easy.validatebag.rules.structural._
import nl.knaw.dans.easy.validatebag.InfoPackageType.{ AIP, SIP }

object ProfileVersion0 {
  def apply(implicit xmlValidators: Map[String, XmlValidator], allowedLicences: Seq[URL]): Seq[NumberedRule] = Seq(
    // BAGIT-RELATED

    // Validity
    NumberedRule("1.1.1", bagMustBeValid, SIP),
    NumberedRule("1.1.2", bagMustBeVirtuallyValid, AIP),

    // bag-info.txt
    NumberedRule("1.2.1", bagMustContainFile(Paths.get("bag-info.txt"))),
    NumberedRule("1.2.2", bagInfoTxtMayContainOne("BagIt-Profile-Version"), dependsOn = Some("1.2.1")),
    NumberedRule("1.2.2", bagInfoTxtElementMustHaveValue("BagIt-Profile-Version", profileVersion0.toString), dependsOn = Some("1.2.2")),
    NumberedRule("1.2.3", bagInfoTxtMayContainOne("BagIt-Profile-URI"), dependsOn = Some("1.2.1")),
    NumberedRule("1.2.3", bagInfoTxtElementMustHaveValue("BagIt-Profile-URI", profileVersion0Uri), dependsOn = Some("1.2.3")),
    NumberedRule("1.2.4", bagInfoTxtMustContainCreated, dependsOn = Some("1.2.1")),
    NumberedRule("1.2.4", bagInfoTxtCreatedMustBeIsoDate, dependsOn = Some("1.2.4")),
    NumberedRule("1.2.5", bagInfoTxtMayContainOne("Is-Version-Of"), dependsOn = Some("1.2.1")),
    NumberedRule("1.2.6", bagInfoTxtMustContainExactlyOne("EASY-User-Account"), AIP, dependsOn = Some("1.2.1")),
    NumberedRule("1.2.6", bagInfoTxtMustNotContain("EASY-User-Account"), SIP, dependsOn = Some("1.2.1")),

    // Manifests
    NumberedRule("1.3.1", bagMustContainSha1PayloadManifest),
    NumberedRule("1.3.1", bagSha1PayloadManifestMustContainAllPayloadFiles, dependsOn = Some("1.3.1")),
    // 1.3.2 does not state restrictions, so it does not need checking

    // STRUCTURAL
    NumberedRule("2.1", bagMustContainDir(Paths.get("metadata"))),
    NumberedRule("2.2", bagMustContainFile(Paths.get("metadata/dataset.xml")), dependsOn = Some("2.1")),
    NumberedRule("2.3", bagMustContainFile(Paths.get("metadata/files.xml")), dependsOn = Some("2.1")),
    NumberedRule("2.4", bagDirectoryMustNotContainAnythingElseThan(Paths.get("metadata"), Seq("dataset.xml", "files.xml")), dependsOn = Some("2.1")),

    // METADATA

    // dataset.xml
    NumberedRule("3.1.1", xmlFileMustConformToSchema(Paths.get("metadata/dataset.xml"), xmlValidators("dataset.xml")), dependsOn = Some("2.2")),
    NumberedRule("3.1.2", ddmMayContainDctermsLicenseFromList(Paths.get("metadata/dataset.xml"), allowedLicences), dependsOn = Some("3.1.1")),
    // TODO: 3.1.3

    // files.xml
    NumberedRule("3.2.1", xmlFileMustConformToSchema(Paths.get("metadata/files.xml"), xmlValidators("files.xml")), dependsOn = Some("2.3")),
    NumberedRule("3.2.2", filesXmlHasDocumentElementFiles, dependsOn = Some("2.3")),
    NumberedRule("3.2.3", filesXmlHasOnlyFiles, dependsOn = Some("3.2.2")),

    NumberedRule("3.2.4", filesXmlFileElementsAllHaveFilepathAttribute, dependsOn = Some("3.2.3")),
    NumberedRule("3.2.4", filesXmlAllFilesDescribedOnce, dependsOn = Some("3.2.4")),
    // 3.2.5 already checked by 3.2.4-rule
    NumberedRule("3.2.6", filesXmlAllFilesHaveFormat, dependsOn = Some("3.2.3")),
//    NumberedRule("3.2.7", filesXmlFilesHaveOnlyDcTerms, dependsOn = Some("3.2.3")), //already checked by XSD
  )
}
