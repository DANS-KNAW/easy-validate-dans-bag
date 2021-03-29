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

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.validatebag.InfoPackageType._
import nl.knaw.dans.easy.validatebag.rules.bagit.closeVerifier
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.joda.time.DateTime
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.Serialization
import org.json4s.{ DefaultFormats, Formats }

import java.nio.file.Paths
import scala.language.reflectiveCalls
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String
  type IsOk = Boolean

  val configuration = Configuration(Paths.get(System.getProperty("app.home")))
  val agent = configuration.properties.getString("http.agent", s"easy-validate-dans-bag/${ configuration.version }")
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

  implicit val formats: Formats = new DefaultFormats {} +
    new EnumNameSerializer(InfoPackageType) +
    EncodingURISerializer

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
        else {
          val maybeBagStore = commandLine.bagStore.toOption
          val packageType = if (commandLine.aip()) AIP
                            else SIP
          if (!commandLine.sipdir()) // single bag
            app.validate(commandLine.bag().toUri, packageType, maybeBagStore).map(formatMsg)
          else {
            val deposits = File(commandLine.bag())
            val now = DateTime.now()
            val rejectedDir = deposits.parent / s"${ deposits.name }-nonvalid-$now"
            val sipToTriedMsg = deposits.list.map { sip =>
              val bagDir = sip.list.filter(_.isDirectory).toSeq.head
              sip -> app.validate(bagDir.toJava.toURI, packageType, maybeBagStore)
            }.toMap
            val violations = sipToTriedMsg.values.filter {
              case Success(ResultMessage(_, _, _, _, false, _)) => true
              case _ => false
            }
            val failures = sipToTriedMsg.values.filter(_.isFailure).map {
              case Failure(e) => e.getMessage
              case _ => "not expected to happen"
            }
            (deposits.parent / s"${ deposits.name }-nonvalid-$now.json")
              .writeText(Serialization.writePretty(violations ++ failures))

            def reject(sip: BagDir) = sip.moveTo(rejectedDir / sip.name)(CopyOptions.atomically)

            sipToTriedMsg.foreach {
              case (sip, Success(ResultMessage(_, _, _, _, false, _))) => reject(sip)
              case (sip, Failure(_)) => reject(sip)
              case _ =>
            }
            val msg: FeedBackMessage = s"violations:${ violations.size }, failures=${ failures.size }; moved to $rejectedDir"
            val isOk: IsOk = violations.isEmpty && failures.isEmpty
            Success(isOk, msg)
          }
        }
      }
  }

  private def formatMsg(msg: ResultMessage) = {
    if (commandLine.responseFormat() == "json") (msg.isCompliant, msg.toJson)
    else (msg.isCompliant, msg.toPlainText)
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
