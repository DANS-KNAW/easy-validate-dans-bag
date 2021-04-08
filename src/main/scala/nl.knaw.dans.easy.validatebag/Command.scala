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
import nl.knaw.dans.easy.validatebag
import nl.knaw.dans.easy.validatebag.InfoPackageType._
import nl.knaw.dans.easy.validatebag.rules.bagit.closeVerifier
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.joda.time.DateTime
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.Serialization
import org.json4s.{ DefaultFormats, Formats }

import java.net.URI
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
  implicit val app: EasyValidateDansBagApp = new EasyValidateDansBagApp(configuration)
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
        else {
          val maybeBagStore = commandLine.bagStore.toOption
          val packageType = if (commandLine.aip()) AIP
                            else SIP
          if (commandLine.sipdir())
            validateBatch(File(commandLine.bag()), packageType, maybeBagStore)(app)
          else app.validate(commandLine.bag().toUri, packageType, maybeBagStore).map(formatMsg)
        }
      }
  }

  private def formatMsg(msg: ResultMessage) = {
    val str = if (commandLine.responseFormat() == "json") msg.toJson
              else msg.toPlainText
    msg.isCompliant -> str
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

  def validateBatch(sipDir: BagDir, packageType: validatebag.InfoPackageType.Value, maybeBagStore: Option[URI])(implicit app: EasyValidateDansBagApp) = {
    val sipToTriedMsg = sipDir.list.map { sip =>
      sip.list.filter(_.isDirectory).toList match {
        case List(bagDir) => sip -> app.validate(bagDir.toJava.toURI, packageType, maybeBagStore)
        case dirs => sip -> Failure(new Exception(s"Expecting one bag directory in $sip, got: ${ dirs.size }"))
      }
    }.toMap.mapValues {
      case Failure(e) => e.getMessage
      case Success(msg) => msg
    }.filter { // drop valid bags
      case (_, ResultMessage(_, _, _, _, true, _)) => false
      case _ => true
    }
    val now = DateTime.now()
    val rejectedDir = sipDir.parent / s"${ sipDir.name }-nonvalid-$now"
    val (violations, failures) = sipToTriedMsg.values
      .partition(_.isInstanceOf[ResultMessage])
    if (sipToTriedMsg.nonEmpty) {
      implicit val formats: Formats = new DefaultFormats {} +
        new EnumNameSerializer(InfoPackageType) +
        EncodingURISerializer
      (sipDir.parent / s"${ sipDir.name }-nonvalid-$now.json")
        .writeText(Serialization.writePretty(sipToTriedMsg.values))
      rejectedDir.createDirectory()
      sipToTriedMsg.keys.foreach(sip =>
        sip.moveTo(rejectedDir / sip.name)(CopyOptions.atomically)
      )
    }
    Success(sipToTriedMsg.isEmpty -> s"violations:${ violations.size }, failures=${ failures.size }; moved to $rejectedDir")
  }
}
