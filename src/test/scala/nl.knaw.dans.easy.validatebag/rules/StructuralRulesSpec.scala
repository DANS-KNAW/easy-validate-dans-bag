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
package nl.knaw.dans.easy.validatebag.rules.structural

import java.nio.file.Paths

import gov.loc.repository.bagit.reader.BagReader
import nl.knaw.dans.easy.validatebag.validation.RuleViolationDetailsException
import nl.knaw.dans.easy.validatebag.{ BagDir, TestSupportFixture }

import scala.util.Failure

class StructuralRulesSpec extends TestSupportFixture {

  val bagReader: BagReader = new BagReader

  val testDirOfMissingMetadata: BagDir = Paths.get("src/test/resources/bags/missingMetadata")
  val testDirOfExistingMetadataNoSpaceNoCapital: BagDir = Paths.get("src/test/resources/bags/existingMetadata")
  val testDirOfExistingMetadataWithSpaces: BagDir = Paths.get("src/test/resources/bags/existingMetadataWithSpaces")
  val testDirOfExistingMetadataContainingUppercaseLetters: BagDir = Paths.get("src/test/resources/bags/existingMetadataContainingUppercaseLetters")

  "bagMustContainMetadataFileV0" should "fail if the file 'metadata' is not found" in {

    val result = bagMustContainMetadataFile(testDirOfMissingMetadata)
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e) => e shouldBe a[RuleViolationDetailsException]
    }
  }

  it should "fail if the file name 'metadata' contains spaces" in {

    val result = bagMustContainMetadataFile(testDirOfExistingMetadataWithSpaces)
    val b: BagDir = Paths.get(testDirOfExistingMetadataWithSpaces.toUri)
    //println(b.resolve("metadata").getFileName.toString)
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e) => e shouldBe a[RuleViolationDetailsException]
    }
  }

  it should "fail if the file name 'metadata' is found but contains uppercase letters" in {

    val result = bagMustContainMetadataFile(testDirOfExistingMetadataContainingUppercaseLetters)
    val b2: BagDir = Paths.get(testDirOfExistingMetadataContainingUppercaseLetters.toUri)
    //println(Files.exists(b2.resolve("metadata")))
    //println(b2.resolve("metadata").toRealPath().getFileName())
    //println(b2.resolve("metadata").toRealPath().getFileName().toString)
    //println(bagReader.read(Paths.get(b2.toUri)))
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e) => e shouldBe a[RuleViolationDetailsException]
    }
  }

  it should "not fail if the file 'metadata' is found and does not contain any spaces or uppercase chars" in {

    val result =  bagMustContainMetadataFile(testDirOfExistingMetadataNoSpaceNoCapital)
    val b1: BagDir = Paths.get(testDirOfExistingMetadataNoSpaceNoCapital.toUri)
    val readBag = bagReader.read(Paths.get(b1.toUri))
    println(b1.resolve("metadata"))
    //print(Files.exists(b1.resolve("metadata")))
    result should not be a[Failure[_]]
  }




}
