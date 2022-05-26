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

import java.net.URI

import nl.knaw.dans.easy.validatebag.InfoPackageType._

class ResultMessageSpec extends TestSupportFixture {

  "constructor" should "fail if isCompliant with violations" in {
    the[IllegalArgumentException] thrownBy
      new ResultMessage(new URI("file://path/to/file.txt"), "file.txt", "0.0.0", SIP, true, Some(Seq("1" -> "Wrong", "2" -> "Even worse"))) should
      have message "requirement failed: when a bag is compliant, no rule violations should be given, or when a bag is not compliant, at least on rule violation should be given"
  }

  it should "fail if not isCompliant without violations" in {
    the[IllegalArgumentException] thrownBy
      new ResultMessage(new URI("file://path/to/file.txt"), "file.txt", "0.0.0", SIP, false, None) should
      have message "requirement failed: when a bag is compliant, no rule violations should be given, or when a bag is not compliant, at least on rule violation should be given"
  }

  it should "fail if not isCompliant with violations, but empty" in {
    the[IllegalArgumentException] thrownBy
      new ResultMessage(new URI("file://path/to/file.txt"), "file.txt", "0.0.0", SIP, false, Some(Seq.empty)) should
      have message "requirement failed: when a bag is compliant, no rule violations should be given, or when a bag is not compliant, at least on rule violation should be given"
  }

  "toPlainText" should "contain violations if present in message object" in {
    val text = ResultMessage(new URI("file://path/to/file.txt"), "file.txt", "0.0.0", SIP, Seq("1" -> "Wrong", "2" -> "Even worse")).toPlainText
    debug(s"Message:\n$text")
    text shouldBe
      """
        |Bag URI: file://path/to/file.txt
        |Bag: file.txt
        |Information package type: SIP
        |Profile version: "0.0.0"
        |Is compliant: false
        |Rule violations:
        | - [1] Wrong
        | - [2] Even worse""".stripMargin
  }

  it should "not contain violations if no present" in {
    val text = ResultMessage(new URI("file://path/to/file.txt"), "file.txt", "0.0.0", SIP).toPlainText
    debug(s"Message:\n$text")
    text shouldBe
      """
        |Bag URI: file://path/to/file.txt
        |Bag: file.txt
        |Information package type: SIP
        |Profile version: "0.0.0"
        |Is compliant: true""".stripMargin
  }

  "toJson" should "contain violations if present in message object" in {
    val json = ResultMessage(new URI("file://path/to/file.txt"), "file.txt", "0.0.0", SIP, Seq("1" -> "Wrong", "2" -> "Even worse")).toJson
    debug(s"Message:\n$json")
    json shouldBe
      """{
        |  "bagUri":"file://path/to/file.txt",
        |  "bag":"file.txt",
        |  "profileVersion":"0.0.0",
        |  "infoPackageType":"SIP",
        |  "isCompliant":false,
        |  "ruleViolations":[
        |    {
        |      "1":"Wrong"
        |    },
        |    {
        |      "2":"Even worse"
        |    }
        |  ]
        |}""".stripMargin
  }

  it should "not contain violations if no present" in {
    val json = ResultMessage(new URI("file://path/to/file.txt"), "file.txt", "0.0.0", SIP).toJson
    debug(s"Message:\n$json")
    json shouldBe
      """{
        |  "bagUri":"file://path/to/file.txt",
        |  "bag":"file.txt",
        |  "profileVersion":0,
        |  "infoPackageType":"SIP",
        |  "isCompliant":true
        |}""".stripMargin
  }
}
