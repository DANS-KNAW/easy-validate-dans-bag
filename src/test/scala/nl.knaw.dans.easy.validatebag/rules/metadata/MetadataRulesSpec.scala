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
import nl.knaw.dans.easy.validatebag.{ CanConnectFixture, Rule, TestSupportFixture, XmlValidator }
import nl.knaw.dans.lib.error._

import scala.util.Try

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

  "xmlFileMustConformToSchema" should "report validation errors if XML not valid" in {
    testRuleViolationRegex(
      rule = xmlFileConformsToSchema(Paths.get("metadata/dataset.xml"), "some schema name", ddmValidator),
      inputBag = "metadata-unknown-element",
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
      inputBag = "metadata-correct",
      doubleCheckBagItValidity = true)
  }

  it should "succeed even if license is specified with https rather than http" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "metadata-correct-license-uri-with-https-scheme",
      doubleCheckBagItValidity = true)
  }

  it should "succeed even if license is specified with a trailing slash" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "metadata-correct-license-uri-with-trailing-slash",
      doubleCheckBagItValidity = true)
  }

  it should "fail if there is no rights holder" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "metadata-license-uri-but-no-rightsholder",
      includedInErrorMsg = "rightsHolder",
      doubleCheckBagItValidity = true)
  }

  it should "fail if the license is not on the list" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "metadata-license-uri-not-on-list",
      includedInErrorMsg = "unknown or unsupported license",
      doubleCheckBagItValidity = true)
  }

  it should "fail if there are two license elements with xsi:type URI" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "metadata-two-license-uris",
      includedInErrorMsg = "Only one license is allowed",
      doubleCheckBagItValidity = true)

  }

  // General syntax will be checked by DDM XML Schema
  "daisAreValid" should "report a DAI that has an invalid check digit" in {
    testRuleViolation(
      rule = ddmDaisAreValid,
      inputBag = "ddm-incorrect-dai",
      includedInErrorMsg = "Invalid DAIs",
      doubleCheckBagItValidity = true)
  }

  it should "accept a DAI with a valid check digit" in {
    testRuleSuccess(
      rule = ddmDaisAreValid,
      inputBag = "ddm-correct-dai")
  }

  "ddmGmlPolygonPosListMustMeetExtraConstraints" should "report error if odd number of values in posList" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-odd-number-of-values",
      includedInErrorMsg = "with odd number of values",
      doubleCheckBagItValidity = true)
  }

  it should "report error if less than 8 values found" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-too-few-values",
      includedInErrorMsg = "too few values",
      doubleCheckBagItValidity = true)
  }

  it should "report error if start and end pair are different" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-start-and-end-different",
      includedInErrorMsg = "unequal first and last pairs",
      doubleCheckBagItValidity = true)
  }

  it should "succeed for correct polygon" in {
    testRuleSuccess(
      rule = ddmGmlPolygonPosListIsWellFormed,
      inputBag = "ddm-poslist-correct",
      doubleCheckBagItValidity = true)
  }

  "polygonsInSameMultiSurfaceMustHaveSameSrsName" should "fail if polygons in the same multi-surface have different srsNames" in {
    testRuleViolation(
      rule = polygonsInSameMultiSurfaceMustHaveSameSrsName,
      inputBag = "ddm-different-srs-names",
      includedInErrorMsg = "Found MultiSurface element containing polygons with different srsNames",
      doubleCheckBagItValidity = true)
  }

  "pointsHaveAtLeastTwoValues" should "fail if a Point with one coordinate is found" in {
    testRuleViolation(
      rule = pointsHaveAtLeastTwoValues,
      inputBag = "ddm-point-with-one-value",
      includedInErrorMsg = "Point with only one coordinate",
      doubleCheckBagItValidity = true)
  }

  it should "fail if a lowerCorner with one coordinate is found" in {
    testRuleViolation(
      rule = pointsHaveAtLeastTwoValues,
      inputBag = "ddm-lowercorner-with-one-value",
      includedInErrorMsg = "Point with only one coordinate",
      doubleCheckBagItValidity = true)
  }

  it should "fail if a upperCorner with one coordinate is found" in {
    testRuleViolation(
      rule = pointsHaveAtLeastTwoValues,
      inputBag = "ddm-uppercorner-with-one-value",
      includedInErrorMsg = "Point with only one coordinate",
      doubleCheckBagItValidity = true)
  }

  "xmlFileMayConformToSchemaIfDefaultNamespace" should "fail if a file element is described twice" in {
    testRuleViolation(
      rule = filesXmlConformsToSchemaIfDeclaredInDefaultNamespace(filesXmlValidator),
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Duplicate unique value",
      doubleCheckBagItValidity = true)
  }

  "filesXmlHasDocumentElementFiles" should "fail if files.xml has document element other than 'files'" in {
    testRuleViolation(
      rule = filesXmlHasDocumentElementFiles,
      inputBag = "filesxml-no-files-as-document-element",
      includedInErrorMsg = "document element must be 'files'",
      doubleCheckBagItValidity = true)
  }

  "filesXmlHasOnlyFiles" should "fail if files.xml/files has non-file child" in {
    testRuleViolation(
      rule = filesXmlHasOnlyFiles,
      inputBag = "filesxml-non-file-element",
      includedInErrorMsg = "non-file elements",
      doubleCheckBagItValidity = true)
  }

  "filesXmlFileElementsAllHaveFilepathAttribute" should "fail if a file element has no filepath attribute" in {
    testRuleViolation(
      rule = filesXmlFileElementsAllHaveFilepathAttribute,
      inputBag = "filesxml-file-element-without-filepath",
      includedInErrorMsg = "Not al 'file' elements have a 'filepath' attribute",
      doubleCheckBagItValidity = true)
  }

  "filesXmlAllFilesDescribedOnce" should "fail if a file is described twice" in {
    testRuleViolation(
      rule = filesXmlAllFilesDescribedOnce,
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Duplicate filepaths found",
      doubleCheckBagItValidity = true)
  }

  it should "fail if a file is not described" in {
    testRuleViolation(
      rule = filesXmlAllFilesDescribedOnce,
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Filepaths in files.xml not equal to files found in data folder",
      doubleCheckBagItValidity = true)
  }

  "filesXmlAllFilesHaveFormat" should "fail if there is a file element without a dct:format child" in {
    testRuleViolation(
      rule = filesXmlAllFilesHaveFormat,
      inputBag = "filesxml-no-dct-format",
      includedInErrorMsg = "not all <file> elements contain a <dcterms:format>",
      doubleCheckBagItValidity = true)
   }

  "filesXmlFilesHaveOnlyDcTerms" should "fail if there is a file element with a non dct child" in {
    testRuleViolation(
      rule = filesXmlFilesHaveOnlyDcTerms,
      inputBag = "filesxml-non-dct-child",
      includedInErrorMsg = "non-dcterms elements found in some file elements",
      doubleCheckBagItValidity = true)
  }

  "all files.xml rules" should "succeed if files.xml is correct" in {
    case class RC(rule: Rule) // Cannot add the rules to a Seq without a container
    Seq[RC](
      RC(filesXmlConformsToSchemaIfDeclaredInDefaultNamespace(filesXmlValidator)),
      RC(filesXmlHasDocumentElementFiles),
      RC(filesXmlHasOnlyFiles),
      RC(filesXmlFileElementsAllHaveFilepathAttribute),
      RC(filesXmlAllFilesDescribedOnce),
      RC(filesXmlAllFilesHaveFormat),
      RC(filesXmlFilesHaveOnlyDcTerms))
      .foreach(rc => testRuleSuccess(rc.rule, inputBag = "metadata-correct", doubleCheckBagItValidity = true))
  }

  // Reusing some test data. This rules is actually not used for files.xml.
  "xmlFileIfExistsMustConformToSchema" should "fail if file exists but does not conform" in {
    testRuleViolation(
      rule = xmlFileIfExistsConformsToSchema(Paths.get("metadata/files.xml"), "files.xml schema", filesXmlValidator),
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Duplicate unique value",
      doubleCheckBagItValidity = true)
  }

  "optionalFileIsUtf8Decodable" should "succeed if file exists and contains valid UTF-8" in {
    testRuleSuccess(
      rule = optionalFileIsUtf8Decodable(Paths.get("bag-info.txt")), // bag-info.txt is not really optional, just using it here for convenience
      inputBag = "minimal")
  }

  it should "succeed if file does NOT exist (as it is OPTIONAL)" in {
    testRuleSuccess(
      rule = optionalFileIsUtf8Decodable(Paths.get("NON-EXISTENT-FILE.TXT")), // bag-info.txt is not really optional, just using it here for convenience
      inputBag = "minimal")
  }

  it should "fail if file contains non-UTF-8 bytes" in {
    testRuleViolation(
      rule = optionalFileIsUtf8Decodable(Paths.get("data/ceci-n-est-pas-d-utf8.jpg")),
      inputBag = "minimal-with-binary-data",
      includedInErrorMsg = "Input not valid UTF-8",
      doubleCheckBagItValidity = true)
  }
}
