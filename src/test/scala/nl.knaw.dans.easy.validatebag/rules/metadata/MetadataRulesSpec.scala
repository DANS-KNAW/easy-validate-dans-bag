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

import java.io.InputStream
import java.net.URI
import java.nio.file.Paths

import nl.knaw.dans.easy.validatebag.InfoPackageType.{ AIP, InfoPackageType, SIP }
import nl.knaw.dans.easy.validatebag._
import nl.knaw.dans.easy.validatebag.rules.ProfileVersion0
import nl.knaw.dans.easy.validatebag.validation.{ RuleViolationDetailsException, RuleViolationException }
import nl.knaw.dans.lib.error._
import org.apache.commons.configuration.PropertiesConfiguration

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

class MetadataRulesSpec extends TestSupportFixture with SchemaFixture with CanConnectFixture {
  private lazy val licensesDir = Paths.get("target/easy-licenses/licenses")
  private lazy val licenses = new PropertiesConfiguration(licensesDir.resolve("licenses.properties").toFile)
    .getKeys.asScala.filterNot(_.isEmpty)
    .map(s => normalizeLicenseUri(new URI(s))).toSeq.collectResults.unsafeGetOrThrow

  "xmlFileConformsToSchema" should "report validation errors if XML not valid" in {
    assume(isAvailable(triedDdmSchema))
    testRuleViolationRegex(
      rule = xmlFileConformsToSchema(Paths.get("metadata/dataset.xml"), "some schema name", ddmValidator),
      inputBag = "ddm-unknown-element",
      includedInErrorMsg = "UNKNOWN-ELEMENT".r
    )
  }

  it should "succeed if XML is valid" in {
    assume(isAvailable(triedDdmSchema))
    testRuleSuccess(
      rule = xmlFileConformsToSchema(Paths.get("metadata/dataset.xml"), "some schema name", ddmValidator),
      inputBag = "metadata-correct")
  }

  "ddmMayContainDctermsLicenseFromList" should "succeed if license is on list" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "metadata-correct")
  }

  it should "succeed if the license is on a separate line" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-correct-license-uri-on-new-line")
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

  it should "succeed if there is no rights holder but license is CC-0" in {
    testRuleSuccess(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-CC-0-license-but-no-rightsholder")
  }

  it should "fail if the license is not on the list" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-license-uri-not-on-list",
      includedInErrorMsg = "unknown or unsupported license")
  }

  it should "fail if the license is not an HTTP or HTTPS URI" in {
    testRuleViolation(
      rule = ddmMayContainDctermsLicenseFromList(licenses),
      inputBag = "ddm-license-not-a-uri",
      includedInErrorMsg = "must be a valid URI")
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

  private val allRules: Seq[NumberedRule] = {
    val xmlValidator = new XmlValidator(null) {
      override def validate(is: InputStream): Try[Unit] = Success(())
    }
    val validatorMap = Map(
      "dataset.xml" -> (if(isAvailable(triedDdmSchema)) ddmValidator else xmlValidator),
      "files.xml" -> (if(isAvailable(triedFileSchema)) filesXmlValidator else xmlValidator),
      "agreements.xml" -> (if(isAvailable(triedAgreementSchema, triedDdmSchema)) agreementsXmlValidator else xmlValidator),
    ) // agreement validation fails at run time when DC schema is not available
    ProfileVersion0.apply(validatorMap, allowedLicences = Seq.empty, BagStore(new URI(""), 1000, 1000))
  }

  private def allRulesBut(nrs: String*) = allRules.filterNot(rule => nrs.contains(rule.nr))

  private def onlyRules(nrs: String*) = allRules.filter(rule => nrs.contains(rule.nr))

  private def aRuleViolation(ruleNumber: RuleNumber, msgs: String*) = {
    if (msgs.length == 1)
      Failure(CompositeException(Seq(RuleViolationException(ruleNumber, msgs.head))))
    else {
      Failure(CompositeException(Seq(RuleViolationException(ruleNumber, compositeMessage(msgs)))))
    }
  }

  private def compositeMessage(msgs: Seq[String]) = {
    CompositeException(msgs.map(RuleViolationDetailsException)).getMessage()
  }

  private def validateRules(bag: TargetBag, infoPackageType: InfoPackageType, rules: Seq[NumberedRule]): Try[Unit] = {
    val ruleNrs = rules.map(r => r.nr).toSet
    val dependencies = rules.flatMap(_.dependsOn).toSet
    if (!dependencies.subsetOf(ruleNrs))
      fail(s"A rule without it dependencies is not executed. rules:$ruleNrs dependencies:$dependencies")
    validation.checkRules(bag, rules, infoPackageType)(isReadable = _.isReadable)
  }

  "new test approach" should "report the not configured license" in {
    val expectedMsg = "Found unknown or unsupported license: http://creativecommons.org/licenses/by-sa/4.0"

    // the next test succeeds with
    // https://github.com/DANS-KNAW/easy-validate-dans-bag/blob/d67357fe306843adbc4b66e960d36f4364ae9228/src/test/scala/nl.knaw.dans.easy.validatebag/EasyValidateDansBagServletSpec.scala#L42
    // that test injects the reported license in the configuration
    validateRules(new TargetBag(bagsDir / "valid-bag", 0), SIP, allRules) shouldBe aRuleViolation("3.1.2", expectedMsg)

    // excluded rules that would cause more errors than the not configured license
    validateRules(new TargetBag(bagsDir / "valid-bag", 0), AIP, allRulesBut("1.2.6(a)", "3.1.3(a)")) shouldBe aRuleViolation("3.1.2", expectedMsg)
  }

  "ddmContainsUrnIdentifier" should "succeed if one or more URN:NBNs are present" in {
    val bag = new TargetBag(bagsDir / "ddm-correct-doi", 0)
    ddmContainsUrnNbnIdentifier(bag) shouldBe Success(())
    val rules = onlyRules("3.1.3(a)", "3.1.1", "2.2(a)", "2.1")
    validateRules(bag, AIP, rules) shouldBe Success(())
    validateRules(bag, SIP, rules) shouldBe Success(())
  }

  it should "fail if there is no URN:NBN-identifier" in {
    val msg = "URN:NBN identifier is missing"
    val bag = new TargetBag(bagsDir / "ddm-missing-urn-nbn", 0)
    val rules = onlyRules("3.1.3(a)", "3.1.1", "2.2(a)", "2.1")
    ddmContainsUrnNbnIdentifier(bag) shouldBe Failure(RuleViolationDetailsException(msg))
    validateRules(bag, SIP, rules) shouldBe Success(())
    validateRules(bag, AIP, rules) shouldBe aRuleViolation("3.1.3(a)", msg)
  }

  "ddmDoiIdentifiersAreValid" should "report invalid DOI-identifiers" in {
    val msg = "Invalid DOIs: 11.1234/fantasy-doi-id, 10/1234/fantasy-doi-id, 10.1234.fantasy-doi-id, http://doi.org/10.1234.567/issn-987-654, https://doi.org/10.1234.567/issn-987-654"
    val bag = new TargetBag(bagsDir / "ddm-incorrect-doi", 0)
    val rules = onlyRules("3.1.3(a)", "3.1.3(b)", "3.1.1", "2.2(a)", "2.1")
    ddmContainsUrnNbnIdentifier(bag) shouldBe Success(())
    ddmDoiIdentifiersAreValid(bag) shouldBe Failure(RuleViolationDetailsException(msg))
    validateRules(bag, AIP, rules) shouldBe aRuleViolation("3.1.3(b)", msg)
    validateRules(bag, SIP, rules) shouldBe aRuleViolation("3.1.3(b)", msg)
  }

  "allUrlsAreValid" should "succeed with valid urls" in {
    testRuleSuccess(
      rule = allUrlsAreValid,
      inputBag = "ddm-correct-urls")
  }

  it should "report all non-valid urls" in {
    testRuleViolation(
      rule = allUrlsAreValid,
      inputBag = "ddm-incorrect-urls",
      includedInErrorMsg =
        """(0) DOI '99.1234.abc' is not valid
          |(1) DOI 'joopajoo' is not valid
          |(2) URN 'uuid:6e8bc430-9c3a-11d9-9669-0800200c9a66' is not valid
          |(3) URN 'niinpä' is not valid
          |(4) protocol 'xttps' in URI 'xttps://www.portable-antiquities.nl/pan/#/object/public/8136' is not one of the accepted protocols [http,https] (value of attribute 'href')
          |(5) protocol 'xttps' in URI 'xttps://data.cultureelerfgoed.nl/term/id/pan/PAN' is not one of the accepted protocols [http,https] (value of attribute 'schemeURI')
          |(6) protocol 'xttps' in URI 'xttps://data.cultureelerfgoed.nl/term/id/pan/17-01-01' is not one of the accepted protocols [http,https] (value of attribute 'valueURI')
          |(7) protocol 'ettp' in URI 'ettp://creativecommons.org/licenses/by-nc-sa/4.0/' is not one of the accepted protocols [http,https]
          |(8) protocol 'xttp' in URI 'xttp://abc.def' is not one of the accepted protocols [http,https]
          |""".stripMargin)
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

  it should "report all invalid points (single coordinate(plain, lower, upper), RD-range)" in {
    // schema validation (3.1.1) is OK, rule 3.1.7 check the ranges
    val expected = aRuleViolation("3.1.7",
      "Point has less than two coordinates: 1.0",
      "Point has less than two coordinates: 1",
      "Point has less than two coordinates: 2",
      "Point is outside RD bounds: -7000 288999",
      "Point is outside RD bounds: 300000 629001",
      "Point is outside RD bounds: -7001 289000",
      "Point is outside RD bounds: 300001 629000",
      "Point has less than two coordinates: 300000",
    )
    val rules = onlyRules("3.1.7", "2.1", "2.2(a)", "3.1.1")
    val bag = new TargetBag(bagsDir / "ddm-invalid-point-values", 0)
    validateRules(bag, AIP, rules) shouldBe expected
    validateRules(bag, SIP, rules) shouldBe expected
  }

  it should "report all invalid points (non-numeric)" in {
    assume(isAvailable(triedFileSchema))
    // schema validation (3.1.1) fails, rule 3.1.7 is not executed
    val expected = aRuleViolation("3.1.1", "metadata/dataset.xml does not conform to DANS dataset metadata schema: " +
      compositeMessage(Seq(
        "cvc-datatype-valid.1.2.1: 'blabla' is not a valid value for 'double'.",
        "cvc-complex-type.2.2: Element 'pos' must have no element [children], and the value must be valid.",
        "cvc-datatype-valid.1.2.1: 'XXX' is not a valid value for 'double'.",
        "cvc-complex-type.2.2: Element 'pos' must have no element [children], and the value must be valid.",
        "cvc-datatype-valid.1.2.1: 'YYY' is not a valid value for 'double'.",
        "cvc-complex-type.2.2: Element 'pos' must have no element [children], and the value must be valid.",
      )))

    val rules = onlyRules("3.1.7", "2.1", "2.2(a)", "3.1.1")
    val bag = new TargetBag(bagsDir / "ddm-invalid-point-syntax", 0)
    validateRules(bag, AIP, rules) shouldBe expected
    validateRules(bag, SIP, rules) shouldBe expected
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
    assume(isAvailable(triedFileSchema))
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

  "filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles" should "fail if a file is described twice" in {
    testRuleViolation(
      rule = filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles,
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Duplicate filepaths found"
    )
  }

  it should "fail if a file is not described" in {
    testRuleViolation(
      rule = filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles,
      inputBag = "filesxml-file-described-twice",
      includedInErrorMsg = "Filepaths in files.xml not equal to files found in data folder"
    )
  }

  it should "succeed when payload files match with fileXML" in {
    testRuleSuccess(
      rule = filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles,
      inputBag = "metadata-correct")
  }

  it should "succeed when payload files combined with file paths in pre-staged.csv match with fileXML" in {
    testRuleSuccess(
      rule = filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles,
      inputBag = "metadata-pre-staged-csv")
  }

  it should "fail when payload files combined with file paths in pre-staged.csv doesn't match with fileXML" in {
    testRuleViolation(
      rule = filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles,
      inputBag = "metadata-pre-staged-csv-one-missing",
      includedInErrorMsg = "Filepaths in files.xml not equal to files found in data folder. Difference -   only in files.xml: {data/leeg3.txt}"
    )
  }

  it should "fail and report about differing file paths in the payload, in pre-staged.csv and in the filesXml" in {
    testRuleViolation(
      rule = filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles,
      inputBag = "metadata-pre-staged-csv-all-three-differ",
      includedInErrorMsg = "Filepaths in files.xml not equal to files found in data folder. Difference - only in bag: {data/leeg5.txt, data/leeg4.txt} only in pre-staged.csv: {data/leeg6.txt} only in files.xml: {data/leeg3.txt}"
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

  "filesXmlFilesHaveOnlyAllowedAccessRights" should "fail if there are access rights values other than those defined in allowedAccessRights" in {
    testRuleViolation(
      rule = filesXmlFilesHaveOnlyAllowedAccessRights,
      inputBag = "filesxml-invalid-access-rights",
      includedInErrorMsg =
        """(0) files.xml: invalid access rights 'open access' in accessRights element for file: 'data/leeg.txt' (allowed values ANONYMOUS, RESTRICTED_REQUEST, NONE)
          |(1) files.xml: invalid access rights 'restricted access' in accessRights element for file: 'data/leeg2.txt' (allowed values ANONYMOUS, RESTRICTED_REQUEST, NONE)
          |(2) files.xml: invalid access rights 'admin' in accessRights element for file: 'data/leeg2.txt' (allowed values ANONYMOUS, RESTRICTED_REQUEST, NONE)""".stripMargin
    )
  }

  it should "succeed when all access rights are valid" in {
    testRuleSuccess(
      rule = filesXmlFilesHaveOnlyAllowedAccessRights,
      inputBag = "filesxml-valid-access-rights")
  }

  it should "succeed when there is no dcterms:accessRight defined" in {
    testRuleSuccess(
      rule = filesXmlFilesHaveOnlyAllowedAccessRights,
      inputBag = "valid-bag")
  }

  "all files.xml rules" should "succeed if files.xml is correct" in {
    Seq[Rule](
      filesXmlHasDocumentElementFiles,
      filesXmlHasOnlyFiles,
      filesXmlFileElementsAllHaveFilepathAttribute,
      filesXmlNoDuplicatesAndMatchesWithPayloadPlusPreStagedFiles,
      filesXmlAllFilesHaveFormat,
      filesXmlFilesHaveOnlyAllowedNamespaces,
      filesXmlFilesHaveOnlyAllowedAccessRights)
      .foreach(testRuleSuccess(_, inputBag = "metadata-correct"))
    assume(isAvailable(triedFileSchema))
    testRuleSuccess(
      filesXmlConformsToSchemaIfFilesNamespaceDeclared(filesXmlValidator),
      inputBag = "metadata-correct"
    )
  }

  // Reusing some test data. This rules is actually not used for files.xml.
  "xmlFileIfExistsConformsToSchema" should "fail if file exists but does not conform" in {
    assume(isAvailable(triedFileSchema))
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
