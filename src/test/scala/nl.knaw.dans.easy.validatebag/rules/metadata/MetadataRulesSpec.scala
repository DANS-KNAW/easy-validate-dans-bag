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
package nl.knaw.dans.easy.validatebag.rules.metadata

import java.net.{ URI, URL }
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import better.files.File
import javax.xml.validation.SchemaFactory
import nl.knaw.dans.easy.validatebag._
import nl.knaw.dans.lib.error._

import scala.util.{ Failure, Try }

class MetadataRulesSpec extends TestSupportFixture with CanConnectFixture {
  private val schemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema")
  private lazy val licenses = File("src/main/assembly/dist/cfg/licenses.txt")
    .contentAsString(StandardCharsets.UTF_8)
    .split("""\s*\n\s*""")
    .filterNot(_.isEmpty)
    .map(s => normalizeLicenseUri(new URI(s))).toSeq.collectResults.unsafeGetOrThrow

  override def beforeEach() {
    assumeCanConnect("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd",
      "http://www.w3.org/2001/03/xml.xsd",
      "http://dublincore.org/schemas/xmls/qdc/2008/02/11/dc.xsd",
      "http://schema.datacite.org/meta/kernel-4/metadata.xsd")
  }

  private lazy val ddmValidator = Try {
    logger.info("Creating ddm.xml validator...")
    val ddmSchema = schemaFactory.newSchema(new URL("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"))
    val v = new XmlValidator(ddmSchema)
    logger.info("ddm.xml validator created.")
    v
  }.unsafeGetOrThrow

  private lazy val filesXmlValidator = Try {
    logger.info("Creating files.xml validator...")
    val filesXmlSchema = schemaFactory.newSchema(new URL("https://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd"))
    val v = new XmlValidator(filesXmlSchema)
    logger.info("files.xml validator created.")
    v
  }.unsafeGetOrThrow

  "xmlFileConformsToSchema" should "report validation errors if XML not valid" in {
    testRuleViolationRegex(
      rule = xmlFileConformsToSchema(Paths.get("metadata/dataset.xml"), "some schema name", ddmValidator),
      inputBag = "ddm-unknown-element",
      includedInErrorMsg = "UNKNOWN-ELEMENT".r
    )
  }

  it should "succeed if XML is valid" in {
    testRuleSuccess(
      rule = xmlFileConformsToSchema(Paths.get("metadata/dataset.xml"), "some schema name", ddmValidator),
      inputBag = "metadata-correct")
  }

  "ddmMayContainDctermsLicenseFromList" should "succeed if license is on list" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "metadata-correct")
  }

  it should "succeed even if license is specified with https rather than http" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-correct-license-uri-with-https-scheme")
  }

  it should "succeed even if license is specified with a trailing slash" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-correct-license-uri-with-trailing-slash")
  }

  it should "fail if there is no rights holder" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-license-uri-but-no-rightsholder",
      includedInErrorMsg = "rightsHolder")
  }

  it should "fail if the license is not on the list" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-license-uri-not-on-list",
      includedInErrorMsg = "unknown or unsupported license")
  }

  it should "fail if there are two license elements with xsi:type URI" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-two-license-uris",
      includedInErrorMsg = "Only one license is allowed")
  }

  // General syntax will be checked by DDM XML Schema
  "ddmDaisAreValid" should "report a DAI that has an invalid check digit" in {
    testRuleViolation(
      rule = ddmDaisAreValid,
      inputBag = "ddm-incorrect-dai",
      includedInErrorMsg = "Invalid DAIs")
  }

  it should "accept a DOI with a valid check digit and prefix" in {
    testRuleSuccess(
      rule = ddmDaisAreValid,
      inputBag = "ddm-correct-dai-with-prefix")
  }

  it should "accept a DAI with a valid check digit" in {
    testRuleSuccess(
      rule = ddmDaisAreValid,
      inputBag = "ddm-correct-dai")
  }

  "ddmContainsValidDoiIdentifier" should "succeed if one or more DOIs are present and they are valid" in {
    testRuleSuccess(
      rule = ddmContainsValidDoiIdentifier,
      inputBag = "ddm-correct-doi")
  }

  it should "fail if there is no DOI-identifier" in {
    testRuleViolation(
      rule = ddmContainsValidDoiIdentifier,
      inputBag = "ddm-missing-doi",
      includedInErrorMsg = "DOI identifier is missing")
  }

  it should "report invalid DOI-identifiers" in {
    testRuleViolation(
      rule = ddmContainsValidDoiIdentifier,
      inputBag = "ddm-incorrect-doi",
      includedInErrorMsg = "Invalid DOIs: 11.1234/fantasy-doi-id, 10/1234/fantasy-doi-id, 10.1234.fantasy-doi-id, http://doi.org/10.1234.567/issn-987-654, https://doi.org/10.1234.567/issn-987-654")
  }

  "ddmGmlPolygonPosListIsWellFormed" should "report error if odd number of values in posList" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-odd-number-of-values",
      includedInErrorMsg = "with odd number of values")
  }

  it should "report error if less than 8 values found" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-too-few-values",
      includedInErrorMsg = "too few values")
  }

  it should "report error if start and end pair are different" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-start-and-end-different",
      includedInErrorMsg = "unequal first and last pairs")
  }

  it should "succeed for correct polygon" in {
    testRuleSuccess(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-correct")
  }

  "polygonsInSameMultiSurfaceHaveSameSrsName" should "fail if polygons in the same multi-surface have different srsNames" in {
    testRuleViolation(
      rule = polygonsInSameMultiSurfaceHaveSameSrsName,
      inputBag = "ddm-different-srs-names",
      includedInErrorMsg = "Found MultiSurface element containing polygons with different srsNames")
  }

  it should "succeed if a MultiSurface element doesn't have any surfaceMember elements in it" in {
    testRuleSuccess(
      rule = polygonsInSameMultiSurfaceHaveSameSrsName,
      inputBag = "ddm-empty-multisurface")
  }

  it should "succeed if a MultiSurface element has surfaceMembers but no srsNames on any of them" in {
    testRuleSuccess(
      rule = polygonsInSameMultiSurfaceHaveSameSrsName,
      inputBag = "ddm-no-srs-names")
  }

  "pointsHaveAtLeastTwoValues" should "fail if a Point with one coordinate is found" in {
    testRuleViolation(
      rule = pointsHaveAtLeastTwoValues,
      inputBag = "ddm-point-with-one-value",
      includedInErrorMsg = "Point with only one coordinate")
  }

  it should "fail if a lowerCorner with one coordinate is found" in {
    testRuleViolation(
      rule = pointsHaveAtLeastTwoValues,
      inputBag = "ddm-lowercorner-with-one-value",
      includedInErrorMsg = "Point with only one coordinate")
  }

  it should "fail if a upperCorner with one coordinate is found" in {
    testRuleViolation(
      rule = pointsHaveAtLeastTwoValues,
      inputBag = "ddm-uppercorner-with-one-value",
      includedInErrorMsg = "Point with only one coordinate")
  }

  "archisIdentifiersHaveAtMost10Characters" should "fail if archis identifiers have values that are too long" in {
    testRuleViolation(
      rule = archisIdentifiersHaveAtMost10Characters,
      inputBag = "ddm-invalid-archis-identifiers",
      includedInErrorMsg =
        """(1) Archis identifier must be 10 or fewer characters long: niet kunnen vinden1
          |(2) Archis identifier must be 10 or fewer characters long: niet kunnen vinden2""".stripMargin
    )
  }

  it should "succeed when no archis identifiers are given" in {
    testRuleSuccess(
      rule = archisIdentifiersHaveAtMost10Characters,
      inputBag = "ddm-no-archis-identifiers",
    )
  }

  it should "succeed with valid archis identifiers" in {
    testRuleSuccess(
      rule = archisIdentifiersHaveAtMost10Characters,
      inputBag = "ddm-valid-archis-identifiers",
    )
  }

  "filesXmlConformsToSchemaIfDeclaredInDefaultNamespace" should "fail if a file element is described twice" in {
    testRuleViolation(
      rule = filesXmlConformsToSchemaIfFilesNamespaceDeclared(filesXmlValidator),
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Duplicate unique value")
  }

  "filesXmlHasDocumentElementFiles" should "fail if files.xml has document element other than 'files'" in {
    testRuleViolation(
      rule = filesXmlHasDocumentElementFiles,
      inputBag = "filesxml-no-files-as-document-element",
      includedInErrorMsg = "document element must be 'files'")
  }

  "filesXmlHasOnlyFiles" should "fail if files.xml/files has non-file child and files.xsd namespace has been declared" in {
    testRuleViolation(
      rule = filesXmlHasOnlyFiles,
      inputBag = "filesxml-non-file-element",
      includedInErrorMsg = "non-file elements")
  }

  "filesXmlFileElementsAllHaveFilepathAttribute" should "fail if a file element has no filepath attribute" in {
    testRuleViolation(
      rule = filesXmlFileElementsAllHaveFilepathAttribute,
      inputBag = "filesxml-file-element-without-filepath",
      includedInErrorMsg = "Not all 'file' elements have a 'filepath' attribute")
  }

  "filesXmlAllFilesDescribedOnce" should "fail if a file is described twice" in {
    testRuleViolation(
      rule = filesXmlAllFilesDescribedOnce,
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Duplicate filepaths found"
    )
  }

  it should "fail if a file is not described" in {
    testRuleViolation(
      rule = filesXmlAllFilesDescribedOnce,
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Filepaths in files.xml not equal to files found in data folder"
    )
  }

  "filesXmlAllFilesHaveFormat" should "fail if there is a file element without a dct:format child" in {
    testRuleViolation(
      rule = filesXmlAllFilesHaveFormat,
      inputBag = "filesxml-no-dct-format",
      includedInErrorMsg = "not all <file> elements contain a <dcterms:format>"
    )
  }

  "filesXmlFilesHaveOnlyDcTerms" should "fail if there is a file element with a non dct child and files.xsd namespace has been declared" in {
    testRuleViolation(
      rule = filesXmlFilesHaveOnlyAllowedNamespaces,
      inputBag = "filesxml-non-dct-child",
      includedInErrorMsg = "non-dc/dcterms elements found in some file elements"
    )
  }

  it should "succeed when the default namespace is used" in {
    testRuleSuccess(
      rule = filesXmlFilesHaveOnlyAllowedNamespaces,
      inputBag = "filesxml-default-namespace-child")
  }

  // NOTE: this test is here to show that invalid elements are accepted here, as long as they're
  // in the dct namespace
  it should "succeed when an invalid element in the dct namespace is used" in {
    testRuleSuccess(
      rule = filesXmlFilesHaveOnlyAllowedNamespaces,
      inputBag = "filesxml-invalid-dct-child")
  }

  // NOTE: this test is here to show that invalid elements are accepted here, as long as they're
  // in the default namespace
  it should "succeed when an invalid element in the default namespace is used" in {
    testRuleSuccess(
      rule = filesXmlFilesHaveOnlyAllowedNamespaces,
      inputBag = "filesxml-invalid-default-namespace-child")
  }

  "all files.xml rules" should "succeed if files.xml is correct" in {
    Seq[Rule](
      filesXmlConformsToSchemaIfFilesNamespaceDeclared(filesXmlValidator),
      filesXmlHasDocumentElementFiles,
      filesXmlHasOnlyFiles,
      filesXmlFileElementsAllHaveFilepathAttribute,
      filesXmlAllFilesDescribedOnce,
      filesXmlAllFilesHaveFormat,
      filesXmlFilesHaveOnlyAllowedNamespaces)
      .foreach(testRuleSuccess(_, inputBag = "metadata-correct"))
  }

  // Reusing some test data. This rules is actually not used for files.xml.
  "xmlFileIfExistsConformsToSchema" should "fail if file exists but does not conform" in {
    testRuleViolation(
      rule = xmlFileIfExistsConformsToSchema(Paths.get("metadata/files.xml"), "files.xml schema", filesXmlValidator),
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Duplicate unique value"
    )
  }

  "optionalFileIsUtf8Decodable" should "succeed if file exists and contains valid UTF-8" in {
    testRuleSuccess(
      rule = optionalFileIsUtf8Decodable(Paths.get("bag-info.txt")), // bag-info.txt is not really optional, just using it here for convenience
      inputBag = "generic-minimal")
  }

  it should "succeed if file does NOT exist (as it is OPTIONAL)" in {
    testRuleSuccess(
      rule = optionalFileIsUtf8Decodable(Paths.get("NON-EXISTENT-FILE.TXT")), // bag-info.txt is not really optional, just using it here for convenience
      inputBag = "generic-minimal")
  }

  it should "fail if file contains non-UTF-8 bytes" in {
    testRuleViolation(
      rule = optionalFileIsUtf8Decodable(Paths.get("data/ceci-n-est-pas-d-utf8.jpg")),
      inputBag = "generic-minimal-with-binary-data",
      includedInErrorMsg = "Input not valid UTF-8")
  }

  it should "fail if an absolute path is inserted" in {
    optionalFileIsUtf8Decodable(Paths.get("/an/absolute/path.jpeg"))(new TargetBag(bagsDir / "generic-minimal-with-binary-data", 0)) should matchPattern {
      case Failure(ae: AssertionError) if ae.getMessage == "assumption failed: Path to UTF-8 text file must be relative." =>
    }
  }
}
