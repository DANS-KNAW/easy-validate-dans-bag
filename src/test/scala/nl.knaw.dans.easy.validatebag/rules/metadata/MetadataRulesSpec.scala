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

import java.net.URL
import java.nio.file.Paths

import javax.xml.validation.SchemaFactory
import nl.knaw.dans.easy.validatebag.{ CanConnectFixture, TestSupportFixture, XmlValidator }
import nl.knaw.dans.lib.error._

import scala.util.Try

class MetadataRulesSpec extends TestSupportFixture with CanConnectFixture {
  private val schemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema")
  private val xsdUrls = Seq("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd", "")

  override def beforeEach() {
    assumeCanConnect("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd",
      "http://www.w3.org/2001/03/xml.xsd",
      "http://dublincore.org/schemas/xmls/qdc/2008/02/11/dc.xsd",
      "http://schema.datacite.org/meta/kernel-4/metadata.xsd")
  }

  private val ddmValidator = Try {
    logger.info("Creating ddm.xml validator...")
    val ddmSchema = schemaFactory.newSchema(new URL("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"))
    val v = new XmlValidator(ddmSchema)
    logger.info("ddm.xml validator created.")
    v
  }.unsafeGetOrThrow

  "xmlFileMustConformToSchema" should "report validation errors if XML not valid" in {
    testRuleViolationRegex(
      rule = xmlFileMustConformToSchema(Paths.get("metadata/dataset.xml"), "some schema name", ddmValidator),
      inputBag = "metadata-unknown-element",
      includedInErrorMsg = "UNKNOWN-ELEMENT".r
    )
  }

  it should "succeed if XML is valid" in {
    testRuleSuccess(
      rule = xmlFileMustConformToSchema(Paths.get("metadata/dataset.xml"), "some schema name", ddmValidator),
      inputBag = "metadata-correct")
  }

  // TODO: TEST ddmMayContainDctermsLicenseFromList

  // General syntax will be checked by DDM XML Schema
  "daisAreValid" should "report a DAI that has an invalid check digit" in {
    testRuleViolation(
      rule = ddmDaisMustBeValid,
      inputBag = "ddm-incorrect-dai",
      includedInErrorMsg = "Invalid DAIs",
      doubleCheckBagItValidity = true)
  }

  // TODO: TEST should accept DAI with valid checkdigit

  "ddmGmlPolygonPosListMustMeetExtraConstraints" should "report error if odd number of values in posList" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListMustMeetExtraConstraints,
      inputBag = "ddm-poslist-odd-number-of-values",
      includedInErrorMsg = "with odd number of values",
      doubleCheckBagItValidity = true
    )
  }

  it should "report error if less than 8 values found" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListMustMeetExtraConstraints,
      inputBag = "ddm-poslist-too-few-values",
      includedInErrorMsg = "too few values",
      doubleCheckBagItValidity = true
    )
  }

  it should "report error if start and end pair are different" in {
    testRuleViolation(
      rule = ddmGmlPolygonPosListMustMeetExtraConstraints,
      inputBag = "ddm-poslist-start-and-end-different",
      includedInErrorMsg = "unequal first and last pairs",
      doubleCheckBagItValidity = true
    )
  }

  it should "succeed for correct polygon" in {
    testRuleSuccess(
      rule = ddmGmlPolygonPosListMustMeetExtraConstraints,
      inputBag = "ddm-poslist-correct",
      doubleCheckBagItValidity = true
    )
  }

  // TODO: TEST polygonsInSameMultiSurfaceMustHaveSameSrsName
  // TODO: TEST pointsHaveAtLeastTwoValues
  // TODO: TEST xmlFileMayConformToSchemaIfDefaultNamespace

  "filesXmlHasDocumentElementFiles" should "fail if files.xml has document element other than 'files'" in {
    testRuleViolation(
      rule = filesXmlHasDocumentElementFiles,
      inputBag = "filesxml-no-files-as-document-element",
      includedInErrorMsg = "document element must be 'files'",
      doubleCheckBagItValidity = true
    )
  }

  // TODO: TEST success if document element is files
  // TODO: TEST filesXmlHasOnlyFiles
  // TODO: TEST filesXmlFileElementsAllHaveFilepathAttribute
  // TODO: TEST filesXmlAllFilesDescribedOnce
  // TODO: TEST filesXmlAllFilesHaveFormat
  // TODO: TEST filesXmlFilesHaveOnlyDcTerms
  // TODO: TEST xmlFileIfExistsMustConformToSchema


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
