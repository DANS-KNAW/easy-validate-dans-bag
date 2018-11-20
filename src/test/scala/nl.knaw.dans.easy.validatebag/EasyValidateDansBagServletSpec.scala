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

import java.net.URI

import com.jayway.jsonpath.internal.JsonReader
import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus.{ BAD_REQUEST_400, OK_200 }
import org.scalatra.test.scalatest.ScalatraSuite

class EasyValidateDansBagServletSpec extends TestSupportFixture with ServletFixture with ScalatraSuite  {
  private val app = new EasyValidateDansBagApp(Configuration("0", createProperties(), Seq(new URI("http://creativecommons.org/licenses/by-sa/4.0"))))
  private val validateBagServlet = new EasyValidateDansBagServlet(app)
  private val jsonReader = new JsonReader()
  addServlet(validateBagServlet, "/*")

  "the validate handler" should "return a 200 and the response when presented a valid bag uri" in {
    post(uri = s"/validate?infoPackageType=SIP&uri=file://${ bagsDir.path.toAbsolutePath }/valid-bag", headers = Seq(("Accept", "application/json"))) {
      status shouldBe OK_200
      info(body)
      jsonReader.parse(body)
      jsonReader.read("$.isCompliant").toString.toBoolean shouldBe true
      jsonReader.read("$.infoPackageType").toString shouldBe "SIP"
    }
  }

  it should "return a 200 and a response including 'compliant: false' and reasons when presented an invalid bag" in {
    post(uri = s"/validate?infoPackageType=SIP&uri=file://${ bagsDir.path.toAbsolutePath }/metadata-correct", headers = Seq(("Accept", "application/json"))) {
      status shouldBe OK_200
      jsonReader.parse(body)
      jsonReader.read("$.bagUri").toString shouldBe s"file://${ bagsDir.path.toAbsolutePath }/metadata-correct"
      jsonReader.read("$.bag").toString shouldBe "metadata-correct"
      jsonReader.read("$.profileVersion").toString shouldBe "0"
      jsonReader.read("$.infoPackageType").toString shouldBe "SIP"
      jsonReader.read("$.ruleViolations.[0]").toString shouldBe "{1.2.4(a)=bag-info.txt must contain exactly one 'Created' element; number found: 0}"
      jsonReader.read("$.ruleViolations.[1]").toString shouldBe "{1.3.1(a)=Mandatory file 'manifest-sha1.txt' not found in bag.}"
    }
  }

  it should "return a 400 if presented a non existing bag uri" in {     //TODO shouldn't this return a 404?
    post(uri = s"/validate?infoPackageType=SIP&uri=file://${ bagsDir.path.toAbsolutePath }/_._metadata-correct", headers = Seq(("Accept", "application/json"))) {
      status shouldBe BAD_REQUEST_400
      body should include("Bag does not exist")
    }
  }

  private def createProperties(): PropertiesConfiguration = {
    val properties = new PropertiesConfiguration()
    properties.setProperty("schemas.ddm", ddmSchemaUrl)
    properties.setProperty("schemas.files", filesSchemaUrl)
    properties.setProperty("schemas.agreements", metadataSchemaUrl)
    properties.setProperty("bagstore-service.base-url", bagsDir.path.toAbsolutePath.toString)
    properties
  }
}
