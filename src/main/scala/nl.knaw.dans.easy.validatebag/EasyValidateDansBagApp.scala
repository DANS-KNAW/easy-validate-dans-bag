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
package nl.knaw.dans.easy.validatebag

import java.net.{ URI, URL }
import java.nio.file.{ Path, Paths }

import better.files.File
import javax.xml.validation.SchemaFactory
import nl.knaw.dans.easy.validatebag.InfoPackageType.InfoPackageType
import nl.knaw.dans.easy.validatebag.rules.{ ProfileVersion0, ProfileVersion1 }
import nl.knaw.dans.easy.validatebag.validation.RuleViolationException
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.xml.sax.SAXParseException

import scala.util.{ Failure, Try }

class EasyValidateDansBagApp(configuration: Configuration) extends DebugEnhancedLogging {
  logger.info("Creating XML Schema factory...")
  private val schemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema")
  logger.info("XML Schema factory created.")

  private def createValidator(schemaUrl: URL): XmlValidator = {
    logger.info(s"Creating validator for $schemaUrl with agent ${ System.getProperty("http.agent") } ...")
    for {
      ddmSchema <- Try(schemaFactory.newSchema(schemaUrl))
      xmlValidator <- Try(new XmlValidator(ddmSchema))
      _ = logger.info(s"Validator created with $schemaUrl")
    } yield xmlValidator
  }.doIfFailure {
    case e: SAXParseException if e.getMessage.contains("Cannot resolve") =>
      logger.error(s"Could not create schema validator (possibly a 3rd party schema is offline or denies access to the user agent) for $schemaUrl : ${ e.getMessage }", e)
    case e =>
      logger.error(s"Could not create validator for $schemaUrl : ${ e.getMessage }", e)
  }.unsafeGetOrThrow

  private val xmlValidators: Map[String, XmlValidator] = Map(
    "dataset.xml" -> createValidator(new URL(configuration.properties.getString("schemas.ddm"))),
    "files.xml" -> createValidator(new URL(configuration.properties.getString("schemas.files"))),
    "agreements.xml" -> createValidator(new URL(configuration.properties.getString("schemas.agreements"))),
    "provenance.xml" -> createValidator(new URL(configuration.properties.getString("schemas.provenance"))),
    "amd.xml" -> createValidator(new URL(configuration.properties.getString("schemas.amd"))),
    "emd.xml" -> createValidator(new URL(configuration.properties.getString("schemas.emd")))
  )

  private val bagStore = BagStore(new URI(configuration.properties.getString("bagstore-service.base-url")),
    configuration.properties.getInt("bagstore-service.connection-timeout-milliseconds", 1000),
    configuration.properties.getInt("bagstore-service.read-timeout-milliseconds", 5000))

  val version: String = configuration.version
  val allRules: Map[ProfileVersion, RuleBase] = {
    Map(
      0 -> ProfileVersion0(xmlValidators, configuration.allowedLicenses, bagStore),
      1 -> ProfileVersion1(xmlValidators, configuration.allowedLicenses))
  }

  def validate(uri: URI, infoPackageType: InfoPackageType, profileVersion: ProfileVersion, bagStoreUrl: Option[URI]): Try[ResultMessage] = {
    bagStore.bagStoreUrl = bagStoreUrl // store name used in deep validation
    val bagName = resolveAndLogBagName(uri)
    for {
      bag <- getBagPath(uri)
      violations <- validation.checkRules(new TargetBag(bag, profileVersion), allRules(profileVersion), infoPackageType)(isReadable = _.isReadable)
        .map(_ => Seq.empty)
        .recoverWith(extractViolations)
      _ = logResult(bagName, violations)
    } yield ResultMessage(uri, bag.getFileName.toString, profileVersion, infoPackageType, violations)
  }

  private def resolveAndLogBagName(uri: URI): String = {
    Try { File(uri).name }
      .doIfSuccess(bagName => logger.info(s"[$bagName]: start validating bag"))
      .doIfFailure { case e => logger.warn(s"${ uri.toString } is a malformed uri, could not resolve the name of the bag dir: ${ e.getMessage }") }
      .getOrElse(uri.toString)
  }

  private def logResult(bagName: String, violations: Seq[(String, String)]): Unit = {
    if (violations.isEmpty) logger.info(s"[$bagName] did not violate any rules and is validated successfully")
    else violations.foreach { case (number: String, message: String) => logger.warn(s"[$bagName] broke rule $number: $message") }
  }

  private def getBagPath(uri: URI): Try[Path] = Try {
    Paths.get(uri.getPath)
  }

  val extractViolations: PartialFunction[Throwable, Try[Seq[(RuleNumber, String)]]] = {
    case x @ CompositeException(xs) =>
      if (xs.forall(_.isInstanceOf[RuleViolationException])) Try(xs.map { case RuleViolationException(nr, details) => (nr, details) })
      else Failure(new IllegalStateException("Rule violations mixed with fatal exceptions. This should not be possible!!!", x))
  }
}
