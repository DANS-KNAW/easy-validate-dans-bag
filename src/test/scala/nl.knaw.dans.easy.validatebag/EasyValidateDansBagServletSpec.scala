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

import org.apache.commons.configuration.PropertiesConfiguration
import org.eclipse.jetty.http.HttpStatus.{ BAD_REQUEST_400, OK_200 }
import org.scalatra.test.scalatest.ScalatraSuite

class EasyValidateDansBagServletSpec extends TestSupportFixture with ServletFixture with ScalatraSuite  {
  private val app = new EasyValidateDansBagApp(Configuration("0", createProperties(), Seq(new URI("http://creativecommons.org/licenses/by-sa/4.0"))))
  private val validateBagServlet = new EasyValidateDansBagServlet(app)
  addServlet(validateBagServlet, "/*")

  "the validate handler" should "return a 200 and the response when presented a valid bag uri" in {
    post(uri = s"/validate?infoPackageType=SIP&uri=file://${ bagsDir.path.toAbsolutePath }/valid-bag", headers = Seq(("Accept", "Application/Json"))) {
      status shouldBe OK_200
      body should include("Is compliant: true")
    }
  }

  it should "return a 200 and a response when presented an invalid bag" in {
    post(uri = s"/validate?infoPackageType=SIP&uri=file://${ bagsDir.path.toAbsolutePath }/metadata-correct", headers = Seq(("Accept", "Application/Json"))) {
      status shouldBe OK_200
      body should include("Is compliant: false")
    }
  }

  it should "return a 400 if presented a non existing bag uri" in {     //TODO shouldn't this return a 404?
    post(uri = s"/validate?infoPackageType=SIP&uri=file://${ bagsDir.path.toAbsolutePath }/_._metadata-correct", headers = Seq(("Accept", "Application/Json"))) {
      status shouldBe BAD_REQUEST_400
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
