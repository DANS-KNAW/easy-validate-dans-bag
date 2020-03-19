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
package nl.knaw.dans.easy

import better.files._
import nl.knaw.dans.easy.validatebag.rules.{ ProfileVersion0, ProfileVersion1 }

import scala.util.{ Failure, Try }

package object validatebag {
  type ProfileVersion = Int
  type RuleNumber = String
  type ErrorMessage = String
  type BagDir = File
  type Rule = TargetBag => Try[Unit]
  type RuleBase = Seq[NumberedRule]

  val profileVersionDois = Map(
    0 -> ProfileVersion0.versionUri,
    1 -> ProfileVersion1.versionUri,
  )

  object InfoPackageType extends Enumeration {
    type InfoPackageType = Value
    val SIP, AIP, BOTH = Value
    
    def fromString(s: String): Try[InfoPackageType] = {
      Try { InfoPackageType.withName(s) }
        .recoverWith { case e => Failure(new IllegalArgumentException(s"invalid InfoPackageType '$s'", e)) }
    }
  }

  import InfoPackageType._

  case class NumberedRule(nr: RuleNumber, rule: Rule, infoPackageType: InfoPackageType = BOTH, dependsOn: List[RuleNumber] = List.empty)
}
