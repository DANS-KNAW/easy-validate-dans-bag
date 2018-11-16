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
    properties.setProperty("schemas.ddm", "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd")
    properties.setProperty("schemas.files", "https://easy.dans.knaw.nl/schemas/bag/metadata/files/2018/04/files.xsd")
    properties.setProperty("schemas.agreements", "https://easy.dans.knaw.nl/schemas/bag/metadata/agreements/2018/05/agreements.xsd")
    properties.setProperty("bagstore-service.base-url", "src/test/resources/bags")
    properties
  }
}
