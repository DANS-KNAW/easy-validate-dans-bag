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

import better.files.File
import gov.loc.repository.bagit.domain.Bag
import gov.loc.repository.bagit.reader.BagReader
import nl.knaw.dans.easy.validatebag.rules.metadata.trace
import nl.knaw.dans.easy.validatebag.rules.structural.originalFilepathsFile
import nl.knaw.dans.easy.validatebag.validation.reject
import nl.knaw.dans.lib.error.TryExtensions
import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}
import resource.managed

import java.nio.charset.Charset
import java.nio.file.Paths
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.collection.{Iterable, Set}
import scala.util.Try
import scala.xml.{Node, Utility, XML}

/**
 * Interface to the bag under validation.
 *
 * Loads resources from the bag lazily and then hangs on to it, so that subsequent rules do not have
 * to reload this information. The location of some resources depends on the profile version. That is
 * why it is provided as an argument to the constructor.
 *
 * @param profileVersion the profile version used
 */
class TargetBag(val bagDir: BagDir, profileVersion: ProfileVersion = 0) {
  private val bagReader = new BagReader()
  private val ddmPath = profileVersion match {
    case 0 => Paths.get("metadata/dataset.xml")
    case 1 => Paths.get("data/pdi/dataset.xml")
  }
  private val filesXmlPath = profileVersion match {
    case 0 => Paths.get("metadata/files.xml")
    case 1 => Paths.get("data/pdi/files.xml")
  }

  lazy val tryBag: Try[Bag] = Try {
    bagReader.read(bagDir.path)
  }

  lazy val tryDdm: Try[Node] = Try {
    Utility.trim {
      XML.loadFile((bagDir / ddmPath.toString).toJava)
    }
  }.recoverWith {
    case t: Throwable => Try {
      reject(s"Unparseable XML: ${t.getMessage}")
    }
  }

  lazy val tryFilesXml: Try[Node] = Try {
    Utility.trim {
      XML.loadFile((bagDir / filesXmlPath.toString).toJava)
    }
  }.recoverWith {
    case t: Throwable => Try {
      reject(s"Unparseable XML: ${t.getMessage}")
    }
  }

  lazy val hasOriginalFilePathsFile = (bagDir / originalFilepathsFile).exists

  lazy val tryOptOriginal2PhysicalFilePath = readPhysicalToOriginalBagRelativePaths()

  def readPhysicalToOriginalBagRelativePaths(): Try[Option[Map[String, String]]] = Try {
    val fileToCheck = bagDir / originalFilepathsFile
    if (fileToCheck.exists)
      Option(fileToCheck.lines.map { line =>
        val list = line.split("""\s+""", 2)
        if (list.size != 2) throw new IllegalArgumentException(s"invalid line in $originalFilepathsFile : $line")
        (list(1), list(0))
      }.toMap)
    else Option.empty
  }

  lazy val preStagedFilePaths = readFilePathsFromCsvFile((bagDir / "metadata/pre-staged.csv"))

  private def readFilePathsFromCsvFile(csvFile: File): Set[String] = {
    if (!csvFile.exists)
      Set.empty
    else
      parseCsv(csvFile).map(line => line.get(0)).filterNot(_.isEmpty).toSet
  }

  private def parseCsv(file: File, nrOfHeaderLines: Int = 1, format: CSVFormat = CSVFormat.RFC4180): Iterable[CSVRecord] = {
    trace(file)
    managed(CSVParser.parse(file.toJava, Charset.forName("UTF-8"), format))
      .map(_.asScala.filter(_.asScala.nonEmpty).drop(nrOfHeaderLines))
      .tried.unsafeGetOrThrow
  }
}
