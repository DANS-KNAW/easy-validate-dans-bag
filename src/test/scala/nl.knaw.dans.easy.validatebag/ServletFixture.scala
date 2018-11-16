package nl.knaw.dans.easy.validatebag

import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.scalatra.test.EmbeddedJettyContainer
import org.scalatra.test.scalatest.ScalatraSuite

/**
 * This Suite relies on Jetty 9.x, while we still require Jetty 8.x
 * By overriding localPort and baseUrl below, issues related to these versions are solved.
 */
trait ServletFixture extends EmbeddedJettyContainer {
  this: ScalatraSuite =>

  override def localPort: Option[Int] = server.getConnectors.collectFirst {
    case x: SelectChannelConnector => x.getLocalPort
  }

  override def baseUrl: String = {
    server.getConnectors.collectFirst {
      case conn: SelectChannelConnector =>
        val host = Option(conn.getHost).getOrElse("localhost")
        val port = conn.getLocalPort
        require(port > 0, "The detected local port is < 1, that's not allowed")
        "http://%s:%d".format(host, port)
    }.getOrElse(sys.error("can't calculate base URL: no connector"))
  }

  def mountServlets(app: EasyValidateDansBagApp): Unit = {
    val validateDansBagServlet = new EasyValidateDansBagServlet(app)
    addServlet(validateDansBagServlet, "/")
  }
}
