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

import nl.knaw.dans.easy.validatebag.InfoPackageType.InfoPackageType
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.Serialization._
import org.json4s.{ CustomSerializer, DefaultFormats, Formats, JNull, JString }

case class ResultMessage(bagUri: URI,
                         bag: String,
                         profileVersion: ProfileVersion,
                         infoPackageType: InfoPackageType,
                         isCompliant: Boolean,
                         ruleViolations: Option[Seq[(String, String)]]) {

  require((isCompliant && ruleViolations.isEmpty) || (!isCompliant && ruleViolations.exists(_.nonEmpty)),
    "when a bag is compliant, no rule violations should be given, " +
      "or when a bag is not compliant, at least on rule violation should be given")

  private implicit val formats: Formats =
    new DefaultFormats {} +
      new EnumNameSerializer(InfoPackageType) +
      EncodingURISerializer

  def toJson(implicit pretty: Boolean = true): String = {
    if (pretty)
      writePretty(this)
    else
      write(this)
  }

  def toPlainText: String = {
    val mandatoryPart =
      s"""
         |Bag URI: ${ bagUri.toASCIIString }
         |Bag: $bag
         |Information package type: $infoPackageType
         |Profile version: $profileVersion
         |Is compliant: $isCompliant""".stripMargin
    val violationsPart = ruleViolations.map(_.map { case (nr, violation) => s" - [$nr] $violation" }.mkString("\nRule violations:\n", "\n", ""))
    mandatoryPart + violationsPart.getOrElse("")
  }
}

object ResultMessage {
  def apply(bagUri: URI,
            bag: String,
            profileVersion: ProfileVersion,
            infoPackageType: InfoPackageType,
            ruleViolations: Seq[(String, String)] = Seq.empty): ResultMessage = {
    new ResultMessage(bagUri, bag, profileVersion, infoPackageType, ruleViolations.isEmpty,
      if (ruleViolations.isEmpty) None
      else Some(ruleViolations))
  }
}

/*
 * Using a custom URI-serializer because the one from json4s extensions does not percent-encode.
 */
case object EncodingURISerializer extends CustomSerializer[URI](format => ( {
  case JString(s) => URI.create(s)
  case JNull => null
}, {
  case x: URI => JString(x.toASCIIString)
}))
