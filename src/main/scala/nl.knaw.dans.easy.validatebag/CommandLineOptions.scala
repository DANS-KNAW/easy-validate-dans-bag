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
import java.nio.file.Path
import org.rogach.scallop.{ ScallopConf, ScallopOption, Subcommand }

class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "easy-validate-dans-bag"
  private val SUBCOMMAND_SEPARATOR = "---\n"
  val description: String = s"""Determines whether a DANS bag is valid according to the DANS BagIt Profile."""
  val synopsis: String =
    s"""
       |  $printedName [--aip] [--bag-store <uri>] [--response-format|-f json|text] [--sipdir] <bag>
       |  $printedName run-service""".stripMargin

  version(s"$printedName v${ configuration.version }")
  banner(
    s"""
       |  $description
       |
       |Usage:
       |
       |$synopsis
       |
       |Options:
       |""".stripMargin)
  val aip: ScallopOption[Boolean] = opt[Boolean]("aip", noshort = true, descr = "Validate the bag(s) as AIP (instead of as SIP)")
  val sipdir: ScallopOption[Boolean] = opt[Boolean]("sipdir", noshort = true, descr = "Validate bags inside directories of trailing argument")
  val bagStore: ScallopOption[URI] = opt[URI]("bag-store", noshort = true, descr = "The bag store to use for deep validation")
  val responseFormat: ScallopOption[String] = opt[String]("response-format", short = 'f', descr = "Format for the result report", default = Some("text"))
  validate(responseFormat) { f =>
    val allowedFormats = Seq("json", "text")
    if (allowedFormats contains f) Right(Unit)
    else Left(s"Format '$f' not one of ${ allowedFormats.mkString(", ") }")
  }

  val bag: ScallopOption[Path] = trailArg[Path]("bag",
    descr = "The bag to validate or in case of '--sipdir': directory with bags in sub directories",
    required = false)

  val runService: Subcommand = new Subcommand("run-service") {
    descr(
      "Starts EASY Validate Dans Bag as a daemon that services HTTP requests")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(runService)

  footer("")
}
