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

import better.files.File
import better.files.File.currentWorkingDirectory
import javax.xml.validation.Schema
import org.scalatest.exceptions.TestFailedException

import scala.util._

class XmlValidatorSpec extends TestSupportFixture with SchemaFixture {

  "Validate" should "return a success when handed a ddm correct xml file" in {
    val xmlFileToTest = currentWorkingDirectory / "src/test/resources/bags/metadata-correct/metadata/dataset.xml"
    assume(isAvailable(triedDdmSchema))
    validateXmlFile(ddmValidator, xmlFileToTest, triedDdmSchema.get) shouldBe a[Success[_]]
  }

  it should "return a failure when handed an incorrect ddm xml file" in {
    val xmlFileToTest = currentWorkingDirectory / "src/test/resources/bags/ddm-incorrect-dai/metadata/dataset.xml"
    assume(isAvailable(triedDdmSchema))
    validateXmlFile(ddmValidator, xmlFileToTest, triedDdmSchema.get) should matchPattern {
      case Failure(tfe: TestFailedException) if tfe.getMessage().contains("does not conform to") =>
    }
  }

  it should "return a failure when handed an incorrect files xml file" in {
    assume(isAvailable(triedDdmSchema))
    val xmlFileToTest = currentWorkingDirectory / "src/test/resources/bags/filesxml-non-file-element/metadata/files.xml"
    validateXmlFile(filesXmlValidator, xmlFileToTest, triedDdmSchema.get) should matchPattern {
      case Failure(tfe: TestFailedException) if tfe.getMessage().contains("does not conform to") =>
    }
  }

  it should "return a success when handed an correct files xml file" in {
    assume(isAvailable(triedDdmSchema))
    val xmlFileToTest = currentWorkingDirectory / "src/test/resources/bags/metadata-correct/metadata/files.xml"
    validateXmlFile(filesXmlValidator, xmlFileToTest, triedDdmSchema.get) shouldBe a[Success[_]]
  }

  private def validateXmlFile(validator: XmlValidator, xmlFileToTest: File, schema: Schema): Try[Unit] = {
    xmlFileToTest.inputStream
      .map(validator.validate(_).recoverWith { case t: Throwable => Try(fail(s"$xmlFileToTest does not conform to $schema: ${ t.getMessage }")) })
      .get()
  }
}
