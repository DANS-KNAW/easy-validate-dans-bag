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

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.logging.servlet._
import org.joda.time.DateTime
import org.scalatra._

import scala.util.{ Failure, Try }

class EasyValidateDansBagServlet(app: EasyValidateDansBagApp) extends ScalatraServlet
  with ServletLogger
  with PlainLogFormatter
  with LogResponseBodyOnError
  with DebugEnhancedLogging {

  get("/") {
    contentType = "text/plain"
    Ok(s"EASY Validate DANS Bag Service running v${ app.version }.")
  }

  post("/validate") {
    val result = for {
      accept <- Try { request.getHeader("Accept") }
      infoPackageType <- Try { InfoPackageType.withName(params.get("infoPackageType").getOrElse("SIP")) }
      uri <- params.get("uri").map(getFileUrl).getOrElse(Failure(new IllegalArgumentException("Required query parameter 'uri' not found")))
      message <- app.validate(uri, infoPackageType)
      body <- Try {
        if (accept == "application/json") message.toJson
        else message.toPlainText
      }
    } yield Ok(body)

    result.getOrRecover {
      case t: IllegalArgumentException =>
        logger.warn(s"validation of bag failed with message: ${ t.getMessage } ")
        BadRequest(s"Input error: ${ t.getMessage }")
      case t =>
        logger.error(s"Server error: ${ t.getMessage }", t)
        InternalServerError(s"[${ new DateTime() }] The server encountered an error. Consult the logs.")
    }
  }

  private def getFileUrl(uriStr: String): Try[URI] = Try {
    val fileUri = new URI(uriStr)
    if (fileUri.getScheme != "file") throw new IllegalArgumentException("Currently only file:/// URLs are supported")
    fileUri
  }
}
