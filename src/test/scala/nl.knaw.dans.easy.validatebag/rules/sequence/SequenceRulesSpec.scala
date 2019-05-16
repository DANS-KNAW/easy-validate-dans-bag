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
package nl.knaw.dans.easy.validatebag.rules.sequence

import java.io.IOException
import java.util.UUID

import nl.knaw.dans.easy.validatebag.{ BagStore, TestSupportFixture }
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success }

class SequenceRulesSpec extends TestSupportFixture with MockFactory {
  private val bagStoreMock = mock[BagStore]

  private def expectUuidDoesNotExist(): Unit = {
    (bagStoreMock.bagExists(_: UUID)) expects * anyNumberOfTimes() returning Success(false)
  }

  private def expectUuidExists(): Unit = {
    (bagStoreMock.bagExists(_: UUID)) expects * anyNumberOfTimes() returning Success(true)
  }

  private def expectBagStoreIoException(): Unit = {
    (bagStoreMock.bagExists(_: UUID)) expects * anyNumberOfTimes() returning Failure(new IOException())
  }

  "bagInfoIsVersionOfIfExistsPointsToArchivedBag" should "fail if UUID not found in bag-store" in {
    expectUuidDoesNotExist()
    testRuleViolation(rule = bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStoreMock), inputBag = "baginfo-with-is-version-of", includedInErrorMsg = "not found in bag stores", doubleCheckBagItValidity = false)
  }

  it should "fail if bag store was not on-line" in {
    expectBagStoreIoException()
    testRuleViolation(rule = bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStoreMock), inputBag = "baginfo-with-is-version-of", includedInErrorMsg = "because of an I/O error", doubleCheckBagItValidity = false)
  }

  it should "succeed if bag was found" in {
    expectUuidExists()
    testRuleSuccess(rule = bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStoreMock), inputBag = "baginfo-with-is-version-of", doubleCheckBagItValidity = false)
  }

  it should "succeed if no Is-Version-Of field is present" in {
    expectUuidDoesNotExist()
    testRuleSuccess(rule = bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStoreMock), inputBag = "baginfo-without-is-version-of", doubleCheckBagItValidity = false)
  }

  it should "fail if a Is-Version-Of field is present without urn" in {
    expectUuidDoesNotExist()
    testRuleViolation(rule = bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStoreMock), inputBag = "baginfo-with-is-version-of-without-urn", includedInErrorMsg = "Is-Version-Of value must be a URN", doubleCheckBagItValidity = false)
  }

  it should "fail if the scheme part does not start with uuid" in {
    expectUuidDoesNotExist()
    testRuleViolation(rule = bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStoreMock), inputBag = "baginfo-with-is-version-of-invalid-scheme", includedInErrorMsg = "Is-Version-Of URN must be of subtype UUID", doubleCheckBagItValidity = false)
  }

  it should "fail if the UUID is NOT in canonical textual representation" in {
    expectUuidDoesNotExist()
    testRuleViolation(rule = bagInfoIsVersionOfIfExistsPointsToArchivedBag(bagStoreMock), inputBag = "baginfo-with-is-version-of-invalid-uuid", includedInErrorMsg = "UUID must be in canonical textual representation", doubleCheckBagItValidity = false)
  }
}
