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
package nl.knaw.dans.easy.validatebag.rules

import java.net.{ URI, URL }
import java.nio.file.Path

import nl.knaw.dans.easy.validatebag.{ TargetBag, XmlValidator }
import nl.knaw.dans.easy.validatebag.validation._

import scala.util.{ Success, Try }
import scala.xml._

package object metadata {
  val namespaceSchemaInstance = new URI("http://www.w3.org/2001/XMLSchema-instance")
  val namespaceDcterms = new URI("http://purl.org/dc/terms/")

  def xmlFileMustConformToSchema(xmlFile: Path, validator: XmlValidator)(t: TargetBag): Try[Unit] = {
    trace(xmlFile)
    require(!xmlFile.isAbsolute, "Path to xmlFile must be relative.")
    (t.bagDir / xmlFile.toString).inputStream.map(validator.validate).map {
      _.recoverWith { case t: Throwable => Try(fail(t.getMessage)) }
    }
  }.get()

  def ddmMayContainDctermsLicenseFromList(ddmPath: Path, allowedLicenses: Seq[URL])(t: TargetBag): Try[Unit] = Try {
    trace(())
    val metadata = t.tryDdm.get \ "dcmiMetadata" // TODO: inside Try
    val licenses = (metadata \ "license").toList
    debug(s"Found licences: ${ licenses.mkString(", ") }")
    lazy val rightsHolders = (metadata \ "rightsHolder").toList

    licenses match {
      case license :: Nil if hasXsiType(namespaceDcterms, "URI")(license) =>
        val licenseURL = new URL(license.text)
        if (allowedLicenses.contains(licenseURL)) {
          if (rightsHolders.isEmpty) fail("Valid license found, but no rightsHolder specified")
          else Success(())
        }
        else fail(s"Found unknown or unsupported license: $licenseURL")
      case Nil | _ :: Nil =>
        debug("No licences with xsi:type=\"dcterms:URI\"")
        Success(())
      case _ => fail(s"Found ${ licenses.size } dcterms:license elements. Only one license is allowed.")
    }
  }

  private def hasXsiType(attrNamespace: URI, attrValue: String)(e: Node): Boolean = {
    e.attribute(namespaceSchemaInstance.toString, "type")
      .exists {
        case Seq(n) =>
          n.text.split(":") match {
            case Array(pref, label) => e.getNamespace(pref) == attrNamespace.toString && label == attrValue
            case _ => false
          }
        case _ => false
      }
  }

  def filesXmlHasDocumentElementFiles(t: TargetBag): Try[Unit] = Try {
    if (t.tryFilesXml.get.label != "files") fail("files.xml: document element of must be 'files'") // TODO: inside Try
  }

  def filesXmlHasOnlyFiles(t: TargetBag): Try[Unit] = Try {
    val files = t.tryFilesXml.get // TODO: inside Try
    val nonFiles = (files \ "*").filterNot(_.label == "file")
    if (nonFiles.nonEmpty) fail(s"files.xml: children of document element must only be 'file'. Found non-file elements: ${ nonFiles.mkString(", ") }")
  }

  def filesXmlFileElementsAllHaveFilepathAttribute(t: TargetBag): Try[Unit] = Try {
    val xml = t.tryFilesXml.get // TODO: inside Try
    val files = xml \ "file"
    if (files.map(_ \@ "filepath").size != files.size) fail("Not all 'file' elements have a 'filepath' attribute")
  }

  def filesXmlAllFilesDescribedOnce(t: TargetBag): Try[Unit] = Try {
    val xml = t.tryFilesXml.get // TODO: inside Try
    val files = xml \ "file"
    val pathsInFilesXmlList = files.map(_ \@ "filepath")
    val duplicatePathsInFilesXml = pathsInFilesXmlList.groupBy(identity).collect { case (k, vs) if vs.size > 1 => k }
    val noDuplicatesFound = duplicatePathsInFilesXml.isEmpty
    val pathsInFileXml = pathsInFilesXmlList.toSet
    val filesInBagPayload = (t.bagDir / "data").walk().filter(_.isRegularFile).toSet
    val payloadPaths = filesInBagPayload.map(t.bagDir.path relativize _).map(_.toString)
    val fileSetsEqual = pathsInFileXml == payloadPaths

    if (noDuplicatesFound && fileSetsEqual) ()
    else {
      val msg =
        (if (noDuplicatesFound) ""
         else s" - Duplicate filepaths found: $duplicatePathsInFilesXml\n") +
          (if (fileSetsEqual) ""
           else s" - Filepaths in files.xml not equal to files found in data folder. Difference: " +
             s"(only in bag: ${ payloadPaths.diff(pathsInFileXml) }, only in files.xml: " +
             s"${ pathsInFileXml.diff(payloadPaths) }\n")
      fail(s"files.xml: errors in filepath-attributes:\n$msg")
    }
  }

  def filesXmlAllFilesHaveFormat(t: TargetBag): Try[Unit] = Try {
    val xml = t.tryFilesXml.get // TODO: inside Try
    val files = xml \ "file"
    val allFilesHaveFormat = files.forall(_.child.exists(n =>
      xml.getNamespace(n.prefix) == "http://purl.org/dc/terms/" && n.label == "format"))
    if (!allFilesHaveFormat) fail("files.xml: not all <file> elements contain a <dcterms:format>")
  }

  //TODO: don't rely on namespace for this, it does not take the substitution into account
  def filesXmlFilesHaveOnlyDcTerms(t: TargetBag): Try[Unit] = Try {
    val xml = t.tryFilesXml.get // TODO: inside Try
    val files = xml \ "file"
    val hasOnlyDcTermsInFileElements = (xml \ "file" \ "_").forall {
      case n: Elem => xml.getNamespace(n.prefix) == "http://purl.org/dc/terms/" || xml.getNamespace(n.prefix) == ""
      case _ => true // Don't check non-element nodes
    }
    if (!hasOnlyDcTermsInFileElements) fail("files.xml: non-dcterms elements found in some file elements")
  }
}
