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
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfter
import org.scalatra.test.scalatest.ScalatraSuite

class EasyValidateDansBagServletSpec extends TestSupportFixture with ServletFixture with ScalatraSuite with MockFactory with BeforeAndAfter {
  private val app = new EasyValidateDansBagApp(Configuration("0", createProperties(), Seq(new URI("http://creativecommons.org/licenses/by-sa/4.0"))))
  private val validateBagServlet = new EasyValidateDansBagServlet(app)
  addServlet(validateBagServlet, "/*")

  "the validate handler" should "return a 200 and the json response when presented a valid bag uri" in {
    post(uri = s"/validate?infoPackageType=SIP&uri=file://${ bagsDir.path.toAbsolutePath }/valid-bag", headers = Seq(("Accept", "Application/Json"))) {

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
