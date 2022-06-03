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
package nl.knaw.dans.easy.validatebag.validation

import better.files.File
import nl.knaw.dans.easy.validatebag.InfoPackageType.SIP
import nl.knaw.dans.easy.validatebag._
import org.apache.commons.configuration.PropertiesConfiguration

import java.net.URI
import java.util.UUID
import scala.util.Success

class BatchSpec extends TestSupportFixture with SchemaFixture {

  "Command.validateBatch" should "" in {
    assume(isAvailable(triedAgreementSchema, triedFileSchema, triedDdmSchema))
    // offline schema's -> no app
    val app: EasyValidateDansBagApp = createApp

    testDir.delete(swallowIOExceptions = true)
    val sipDir = testDir / "input"
    (sipDir / "empty").createDirectories()
    (sipDir / "multiple" / "1").createDirectories()
    (sipDir / "multiple" / "2").createDirectories()
    File("src/test/resources/bags/valid-bag")
      .copyTo(sipDir / "valid-bag" / UUID.randomUUID().toString)
    File("src/test/resources/bags/baginfo-missing-bag-infotxt")
      .copyTo(sipDir / "invalid-bag" / UUID.randomUUID().toString)

    Command.validateBatch(sipDir, SIP, 0, None)(app) should matchPattern {
      case Success((false, msg)) if msg.toString.startsWith(
        s"violations:1, failures=2; moved to $sipDir-nonvalid-20"
      ) =>
    }
    sipDir.list.toList.map(_.name) shouldBe List("valid-bag")
    testDir.list.filterNot(_ == sipDir)
      .filter(_.isDirectory).toList.head // the dir with rejected bags
      .list.toList.map(_.name) should
      contain only("empty", "invalid-bag", "multiple")
    val q = "\"" // https://docs.scala-lang.org/sips/interpolation-quote-escape.html
    testDir.list.filterNot(_.isDirectory).toList.head.contentAsString should
      (include(s"Expecting one bag directory in $sipDir/empty, got: 0") and
        include(s"Expecting one bag directory in $sipDir/multiple, got: 2") and
        include(s"${q}bagUri$q:${q}file:$sipDir/invalid-bag/") and
        include(
          """    "profileVersion":0,
            |    "infoPackageType":"SIP",
            |    "isCompliant":false,
            |    "ruleViolations":[
            |      {
            |        "1.2.1":"Mandatory file 'bag-info.txt' not found in bag."
            |      },
            |      {
            |        "2.1":"Mandatory directory 'metadata' not found in bag."
            |      }
            |    ]
            |""".stripMargin)
        )
  }

  private def createApp = {
    new EasyValidateDansBagApp(Configuration(
      version = "1.0.0",
      new PropertiesConfiguration() {
        setProperty("schemas.ddm", ddmSchemaUrl)
        setProperty("schemas.files", filesSchemaUrl)
        setProperty("schemas.agreements", agreementsSchemaUrl)
        setProperty("schemas.provenance", provenanceSchemaUrl)
        setProperty("schemas.amd", amdSchemaUrl)
        setProperty("schemas.emd", emdSchemaUrl)
        setProperty("bagstore-service.base-url", bagsDir.path.toAbsolutePath.toString)
      },
      Seq(new URI("http://creativecommons.org/licenses/by-sa/4.0")),
    ))
  }
}
