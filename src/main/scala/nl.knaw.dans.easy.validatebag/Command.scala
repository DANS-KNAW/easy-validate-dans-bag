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

import java.nio.file.Paths

import nl.knaw.dans.easy.validatebag.InfoPackageType._
import nl.knaw.dans.easy.validatebag.rules.bagit.closeVerifier
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls
import scala.util.{ Failure, Try }
import scala.util.control.NonFatal

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String
  type IsOk = Boolean

  val configuration = Configuration(Paths.get(System.getProperty("app.home")))
  val agent = configuration.properties.getString("http.agent","EasyValidateDansBag")
  logger.info(s"setting http.agent to $agent")
  System.setProperty("http.agent", agent)

  debug("Parsing command line...")
  val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }
  debug("Creating application object...")
  val app = new EasyValidateDansBagApp(configuration)
  debug(s"Executing command line: ${ args.mkString(" ") }")
  runSubcommand(app).doIfSuccess { case (ok, msg) =>
    if (ok) Console.err.println(s"OK: $msg")
    else Console.err.println(s"FAILED: $msg")
  }
    .doIfFailure { case e => logger.error(e.getMessage, e) }
    .doIfFailure { case NonFatal(e) => Console.err.println(s"FAILED: ${ e.getMessage }") }

  closeVerifier()

  private def runSubcommand(app: EasyValidateDansBagApp): Try[(IsOk, FeedBackMessage)] = {
    commandLine.subcommand
      .collect {
        case commandLine.runService =>
          debug("Running as service...")
          runAsService(app)
      }
      .getOrElse {
        // Validate required parameter here, because it is only required when not running as service.
        if (commandLine.bag.isEmpty) Failure(new IllegalArgumentException("Parameter 'bag' required if not running as a service"))
        else
          app.validate(commandLine.bag().toUri, if (commandLine.aip()) AIP
                                                else SIP, commandLine.bagStore.toOption).map {
            msg =>
              if (commandLine.responseFormat() == "json") (msg.isCompliant, msg.toJson)
              else (msg.isCompliant, msg.toPlainText)
          }
      }
  }

  private def runAsService(app: EasyValidateDansBagApp): Try[(IsOk, FeedBackMessage)] = Try {
    val service = new EasyValidateDansBagService(configuration.properties.getInt("daemon.http.port"), app)
    Runtime.getRuntime.addShutdownHook(new Thread("service-shutdown") {
      override def run(): Unit = {
        service.stop()
        service.destroy()
      }
    })

    service.start()
    Thread.currentThread.join()
    (true, "Service terminated normally.")
  }
}
