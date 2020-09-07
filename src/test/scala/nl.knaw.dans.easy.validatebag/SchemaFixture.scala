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
package nl.knaw.dans.easy.validatebag

import java.net.{ URL, UnknownHostException }

import javax.xml.validation.{ Schema, SchemaFactory }

import scala.util.{ Failure, Try }
import scala.xml.SAXParseException

trait SchemaFixture {
  val ddmSchemaUrl = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  val filesSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/files/2018/04/files.xsd"
  val metadataSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/agreements/2018/12/agreements.xsd"

  private lazy val schemaFactory: SchemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema")
  lazy val triedDdmSchema: Try[Schema] = loadSchema(ddmSchemaUrl)
  lazy val triedFileSchema: Try[Schema] = loadSchema(filesSchemaUrl)
  lazy val triedMetadataSchema: Try[Schema] = loadSchema(metadataSchemaUrl)

  def ddmValidator: XmlValidator = validator(triedDdmSchema)

  def filesXmlValidator: XmlValidator = validator(triedFileSchema)

  private def validator(triedSchema: Try[Schema]): XmlValidator = {
    assume(isAvailable(triedSchema)) // fall back if the test forgets the assume
    Try(new XmlValidator(triedSchema.get)).get
  }

  private def loadSchema(url: RuleNumber) = {
    Try(schemaFactory.newSchema(new URL(url)))
  }

  def isAvailable(triedSchemas: Try[Schema]*): Boolean = triedSchemas.forall {
    case Failure(e: SAXParseException) if e.getCause.isInstanceOf[UnknownHostException] =>
      println("UnknownHostException: " + e.getMessage)
      false
    case Failure(e: SAXParseException) if e.getMessage.contains("Cannot resolve") =>
      println("Probably an offline third party schema: " + e.getMessage)
      false
    case _ => true
  }
}
