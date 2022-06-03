/*
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

import nl.knaw.dans.easy.validatebag.InfoPackageType.AIP
import nl.knaw.dans.easy.validatebag.rules.bagit._
import nl.knaw.dans.easy.validatebag.rules.metadata._
import nl.knaw.dans.easy.validatebag.rules.structural._
import nl.knaw.dans.easy.validatebag.{ NumberedRule, XmlValidator }

import java.net.URI
import java.nio.file.Paths

object ProfileVersion1 {
  def apply(implicit xmlValidators: Map[String, XmlValidator], allowedLicences: Seq[URI]): Seq[NumberedRule] = Seq(
    // BAGIT-RELATED

    // Validity
    NumberedRule("1.1.1", bagIsValid),
    NumberedRule("1.1.1(datadir)", containsDir(Paths.get("data"))),

    // bag-info.txt
    NumberedRule("1.2.1", bagInfoExistsAndIsWellFormed),
    NumberedRule("1.2.4(a)", bagInfoContainsExactlyOneOf("Created"), dependsOn = List("1.2.1")),
    NumberedRule("1.2.4(b)", bagInfoCreatedElementIsIso8601Date, dependsOn = List("1.2.4(a)")),
    NumberedRule("1.2.5", bagInfoContainsAtMostOneOf("Is-Version-Of"), dependsOn = List("1.2.1")), // TODO: check that value is urn:uuid ?

    // Manifests
    NumberedRule("1.3.1(a)", containsFile(Paths.get("manifest-sha1.txt")), AIP),
    NumberedRule("1.3.1(b)", bagShaPayloadManifestContainsAllPayloadFiles, AIP, dependsOn = List("1.2.1", "1.3.1(a)")),
    // 1.3.2 does not state restrictions, so it does not need checking

    // STRUCTURAL
    NumberedRule("2.1", containsDir(Paths.get("metadata"))),
    NumberedRule("2.2(a)", containsFile(Paths.get("metadata/dataset.xml")), dependsOn = List("2.1")),
    NumberedRule("2.2(b)", containsFile(Paths.get("metadata/files.xml")), dependsOn = List("2.1")),
    // 2.3 does not state restrictions, so it does not need checking
    NumberedRule("2.5", containsNothingElseThan(
      Paths.get("metadata"),
      Seq(
        "dataset.xml",
        "provenance.xml",
        "pre-staged.csv",
        "files.xml",
        "amd.xml",
        "emd.xml",
        "license.txt",
        "license.pdf",
        "license.html",
        "depositor-info",
        "original",
        "original/dataset.xml",
        "original/files.xml",
        "depositor-info/agreements.xml",
        "depositor-info/message-from-depositor.txt",
        "depositor-info/depositor-agreement.pdf",
        "depositor-info/depositor-agreement.txt",
      )
    ), dependsOn = List("2.1")),
    NumberedRule("2.6", hasOnlyValidFileNames, dependsOn = List("1.3.1(b)")),
    NumberedRule("2.7.1", optionalFileIsUtf8Decodable(Paths.get(originalFilepathsFile))),
    NumberedRule("2.7.2", isOriginalFilepathsFileComplete, dependsOn = List("1.1.1(datadir)", "2.7.1", "2.2(b)", "3.2.4")),

    // METADATA

    // dataset.xml
    NumberedRule("3.1.1", xmlFileConformsToSchema(Paths.get("metadata/dataset.xml"), "DANS dataset metadata schema", xmlValidators("dataset.xml")), dependsOn = List("2.2(a)")),
    NumberedRule("3.1.2", ddmMayContainDctermsLicenseFromList(allowedLicences), dependsOn = List("3.1.1")),
    NumberedRule("3.1.3(a)", ddmContainsUrnNbnIdentifier, AIP, dependsOn = List("3.1.1")),
    NumberedRule("3.1.3(b)", ddmDoiIdentifiersAreValid, dependsOn = List("3.1.1")),
    NumberedRule("3.1.4", ddmDaisAreValid, dependsOn = List("3.1.1")),
    NumberedRule("3.1.5", ddmGmlPolygonPosListIsWellFormed, dependsOn = List("3.1.1")),
    NumberedRule("3.1.6", polygonsInSameMultiSurfaceHaveSameSrsName, dependsOn = List("3.1.1")),
    NumberedRule("3.1.7", pointsHaveAtLeastTwoValues, dependsOn = List("3.1.1")),
    NumberedRule("3.1.8", archisIdentifiersHaveAtMost10Characters, dependsOn = List("3.1.1")),
    NumberedRule("3.1.9", allUrlsAreValid, dependsOn = List("3.1.1")),
    NumberedRule("3.1.10", ddmMustHaveRightsHolder, dependsOn = List("3.1.1")),

    // files.xml
    NumberedRule("3.2.1", xmlFileConformsToSchema(Paths.get("metadata/files.xml"), "DANS dataset file list schema", xmlValidators("files.xml")), dependsOn = List("2.2(b)")),
    NumberedRule("3.2.2", filesXmlHasDocumentElementFiles, dependsOn = List("2.2(b)")),
    NumberedRule("3.2.3", filesXmlHasOnlyFiles, dependsOn = List("3.2.2")),
    NumberedRule("3.2.4", filesXmlFileElementsAllHaveFilepathAttribute, dependsOn = List("3.2.3")),
    NumberedRule("3.2.5", filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles, dependsOn = List("1.1.1(datadir)", "3.2.4")),

    // TODO: redundant? It seems to check part of 2.7.2 again
    NumberedRule("2.7.2(b)", filesXmlFileElementsInOriginalFilePaths, dependsOn = List("3.2.3", "2.7.1", "2.2(b)", "3.2.4")),

    NumberedRule("3.2.6", filesXmlAllFilesHaveFormat, dependsOn = List("3.2.2")),
    NumberedRule("3.2.7", filesXmlFilesHaveOnlyAllowedNamespaces, dependsOn = List("3.2.2")),
    NumberedRule("3.2.8", filesXmlFilesHaveOnlyAllowedAccessRights, dependsOn = List("3.2.2")),

    // agreements.xml
    NumberedRule("3.3.1", xmlFileIfExistsConformsToSchema(Paths.get("metadata/depositor-info/agreements.xml"), "Agreements metadata schema", xmlValidators("agreements.xml")), infoPackageType = AIP),

    // amd.xml
    NumberedRule("3.6.1", xmlFileIfExistsConformsToSchema(Paths.get("metadata/amd.xml"), "EASY Administrative Metadata", xmlValidators("amd.xml")), infoPackageType = AIP),

    // emd.xml
    NumberedRule("3.7.1", xmlFileIfExistsConformsToSchema(Paths.get("metadata/emd.xml"), "EASY Metadata", xmlValidators("emd.xml")), infoPackageType = AIP),

    // emd.xml
    NumberedRule("3.8.1", xmlFileIfExistsConformsToSchema(Paths.get("metadata/provenance.xml"), "DANS Provenance Metadata", xmlValidators("provenance.xml")), infoPackageType = AIP),
  )
}
