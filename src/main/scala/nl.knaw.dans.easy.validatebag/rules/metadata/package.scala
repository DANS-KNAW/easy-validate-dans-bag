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

import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.{ CharacterCodingException, Charset }
import java.nio.file.{ Path, Paths }

import nl.knaw.dans.easy.validatebag.validation._
import nl.knaw.dans.easy.validatebag.{ TargetBag, XmlValidator }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.matching.Regex
import scala.util.{ Failure, Success, Try }
import scala.xml._

package object metadata extends DebugEnhancedLogging {
  val filesXmlNamespace = "http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
  val dcxDaiNamespace = "http://easy.dans.knaw.nl/schemas/dcx/dai/"
  val identifierTypeNamespace = "http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
  val gmlNamespace = "http://www.opengis.net/gml"
  val dcNamespace = "http://purl.org/dc/elements/1.1/"
  val dctermsNamespace = "http://purl.org/dc/terms/"
  val schemaInstanceNamespace = "http://www.w3.org/2001/XMLSchema-instance"

  val allowedFilesXmlNamespaces = List(dcNamespace, dctermsNamespace)

  val doiPattern: Regex = raw"^10(\.\d+)+/.+".r

  val daiPrefix = "info:eu-repo/dai/nl/"

  def xmlFileIfExistsConformsToSchema(xmlFile: Path, schemaName: String, validator: XmlValidator)
                                     (t: TargetBag): Try[Unit] = {
    trace(xmlFile)
    assume(!xmlFile.isAbsolute, "Path to xmlFile must be relative.")
    if ((t.bagDir / xmlFile.toString).exists) xmlFileConformsToSchema(xmlFile, schemaName, validator)(t)
    else Success(())
  }

  def xmlFileConformsToSchema(xmlFile: Path, schemaName: String, validator: XmlValidator)
                             (t: TargetBag): Try[Unit] = {
    trace(xmlFile)
    assume(!xmlFile.isAbsolute, "Path to xmlFile must be relative.")
    (t.bagDir / xmlFile.toString).inputStream.map(validator.validate).map {
      _.recoverWith { case t: Throwable => Try(fail(s"$xmlFile does not conform to $schemaName: ${ t.getMessage }")) }
    }
  }.get()

  def filesXmlConformsToSchemaIfFilesNamespaceDeclared(validator: XmlValidator)
                                                      (t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.flatMap {
      xml =>
        if (xml.namespace == filesXmlNamespace) {
          logger.debug("Validating files.xml against XML Schema")
          xmlFileConformsToSchema(Paths.get("metadata/files.xml"), "files.xml", validator)(t)
        }
        else {
          logger.info(s"files.xml does not declare namespace ${ filesXmlNamespace }, NOT validating with XML Schema")
          Success(())
        }
    }
  }

  def ddmMayContainDctermsLicenseFromList(allowedLicenses: Seq[URI])(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryDdm.map {
      ddm =>
        val metadata = ddm \ "dcmiMetadata"
        val licenses = (metadata \ "license").toList
        debug(s"Found licences: ${ licenses.mkString(", ") }")
        lazy val rightsHolders = (metadata \ "rightsHolder").toList
        licenses match {
          case license :: Nil if hasXsiType(dctermsNamespace, "URI")(license) =>
            val licenseUri = normalizeLicenseUri(new URI(license.text)).get
            if (!allowedLicenses.contains(licenseUri)) fail(s"Found unknown or unsupported license: $licenseUri")
            if (rightsHolders.isEmpty) fail("Valid license found, but no rightsHolder specified")
          case Nil | _ :: Nil =>
            debug("No licences with xsi:type=\"dcterms:URI\"")
          case _ => fail(s"Found ${ licenses.size } dcterms:license elements. Only one license is allowed.")
        }
    }
  }

  def ddmContainsValidDoiIdentifier(t: TargetBag): Try[Unit] = {
    for {
      ddm <- t.tryDdm
      dois <- getDoiIdentifiers(ddm)
      _ <- doisAreSyntacticallyValid(dois)
    } yield ()
  }

  private def getDoiIdentifiers(ddm: Node): Try[Seq[String]] = Try {
    val dois = (ddm \\ "identifier").filter(hasXsiType(identifierTypeNamespace, "DOI"))
    if (dois.isEmpty) fail("DOI identifier is missing")
    dois.map(_.text)
  }

  private def doisAreSyntacticallyValid(dois: Seq[String]): Try[Unit] = Try {
    logger.debug(s"DOIs to check: ${ dois.mkString(", ") }")
    val invalidDois = dois.filterNot(syntacticallyValidDoi)
    if (invalidDois.nonEmpty) fail(s"Invalid DOIs: ${ invalidDois.mkString(", ") }")
  }

  private def syntacticallyValidDoi(doi: String): Boolean = {
    doiPattern.findFirstIn(doi).nonEmpty
  }

  /**
   * Converts all license URIs to one with scheme http and without any trailing slashes. Technically, these are not the same URIs but
   * for the purpose of identifying licenses this is good enough.
   *
   * @param uri the URI to normalize
   * @return normalized URI
   */
  def normalizeLicenseUri(uri: URI): Try[URI] = Try {
    def normalizeLicenseUriPath(p: String): String = {
      val nTrailingSlashes = p.toCharArray.reverse.takeWhile(_ == '/').length
      p.substring(0, p.length - nTrailingSlashes)
    }

    def normalizeLicenseUriScheme(s: String): String = {
      if (s == "http" || s == "https") "http"
      else throw new IllegalArgumentException(s"Only http or https license URIs allowed. URI scheme found: $s")
    }

    new URI(normalizeLicenseUriScheme(uri.getScheme), uri.getUserInfo, uri.getHost, uri.getPort, normalizeLicenseUriPath(uri.getPath), uri.getQuery, uri.getFragment)
  }

  private def hasXsiType(attrNamespace: String, attrValue: String)(e: Node): Boolean = {
    e.attribute(schemaInstanceNamespace.toString, "type")
      .exists {
        case Seq(n) =>
          n.text.split(":") match {
            case Array(pref, label) => e.getNamespace(pref) == attrNamespace && label == attrValue
            case _ => false
          }
        case _ => false
      }
  }

  def ddmDaisAreValid(t: TargetBag): Try[Unit] = {
    for {
      ddm <- t.tryDdm
      _ <- daisAreValid(ddm)
    } yield ()
  }

  private def daisAreValid(ddm: Node): Try[Unit] = Try {
    val dais = (ddm \\ "DAI").filter(_.namespace == dcxDaiNamespace)
    logger.debug(s"DAIs to check: ${ dais.mkString(", ") }")
    val invalidDais = dais.map(_.text.stripPrefix(daiPrefix)).filterNot(s => digest(s.slice(0, s.length - 1), 9) == s.last)
    if (invalidDais.nonEmpty) fail(s"Invalid DAIs: ${ invalidDais.mkString(", ") }")
  }

  // Calculated the check digit of a DAI. Implementation copied from easy-ddm.
  private def digest(message: String, modeMax: Int): Char = {
    val reverse = message.reverse
    var f = 2
    var w = 0
    var mod = 0
    mod = 0
    while ( { mod < reverse.length }) {
      val cx = reverse.charAt(mod)
      val x = cx - 48
      w += f * x
      f += 1
      if (f > modeMax) f = 2

      { mod += 1; mod }
    }
    mod = w % 11
    if (mod == 0) '0'
    else {
      val c = 11 - mod
      if (c == 10) 'X'
      else (c + 48).toChar
    }
  }

  def ddmGmlPolygonPosListIsWellFormed(t: TargetBag): Try[Unit] = {
    trace(())
    for {
      ddm <- t.tryDdm
      posLists <- getPolygonPosLists(ddm)
      _ <- posLists.map(validatePosList).collectResults.recoverWith {
        case ce: CompositeException => Try(fail(ce.getMessage))
      }
    } yield ()
  }

  private def getPolygonPosLists(parent: Node): Try[Seq[Node]] = Try {
    trace(())
    val polygons = getPolygons(parent)
    polygons.flatMap(_ \\ "posList")
  }

  private def getPolygons(parent: Node): NodeSeq = {
    (parent \\ "Polygon").filter(_.namespace == gmlNamespace)
  }

  private def validatePosList(node: Node): Try[Unit] = Try {
    trace(node)

    def offendingPosListMsg(values: Seq[String]): String = {
      s"(Offending posList starts with: ${ values.take(10).mkString(", ") }...)"
    }

    val values = node.text.split("""\s+""").toList
    val numberOfValues = values.size
    if (numberOfValues % 2 != 0) fail(s"Found posList with odd number of values: $numberOfValues. ${ offendingPosListMsg(values) }")
    if (numberOfValues < 8) fail(s"Found posList with too few values (fewer than 4 pairs). ${ offendingPosListMsg(values) }")
    if (values.take(2) != values.takeRight(2)) fail(s"Found posList with unequal first and last pairs. ${ offendingPosListMsg(values) }")
  }

  def polygonsInSameMultiSurfaceHaveSameSrsName(t: TargetBag): Try[Unit] = {
    trace(())
    val result = for {
      ddm <- t.tryDdm
      multiSurfaces <- getMultiSurfaces(ddm)
      _ <- multiSurfaces.map(validateMultiSurface).collectResults
    } yield ()

    result.recoverWith {
      case ce: CompositeException => Try(fail(ce.getMessage))
    }
  }

  private def getMultiSurfaces(ddm: Node): Try[NodeSeq] = Try {
    (ddm \\ "MultiSurface").filter(_.namespace == gmlNamespace)
  }

  private def validateMultiSurface(ms: Node): Try[Unit] = {
    val polygons = getPolygons(ms)
    if (polygons.isEmpty || polygons.flatMap(_.attribute("srsName").map(_.text)).distinct.size <= 1) Success(())
    else Try(fail("Found MultiSurface element containing polygons with different srsNames"))
  }

  def pointsHaveAtLeastTwoValues(t: TargetBag): Try[Unit] = {
    trace(())
    val result = for {
      ddm <- t.tryDdm
      points <- getGmlPoints(ddm)
      _ <- points.map(validatePoint).collectResults
    } yield ()

    result.recoverWith {
      case ce: CompositeException => Try(fail(ce.getMessage))
    }
  }

  private def getGmlPoints(ddm: Node): Try[NodeSeq] = Try {
    ((ddm \\ "Point") ++ (ddm \\ "lowerCorner") ++ (ddm \\ "upperCorner")).filter(_.namespace == gmlNamespace)
  }

  private def validatePoint(point: Node): Try[Unit] = {
    val coordinates = point.text.trim.split("""\s+""")
    if (coordinates.length > 1) Success(())
    else Try(fail(s"Point with only one coordinate: ${ point.text.trim }"))
  }

  def archisIdentifiersHaveAtMost10Characters(t: TargetBag): Try[Unit] = {
    trace(())
    for {
      ddm <- t.tryDdm
      identifiers = getArchisIdentifiers(ddm)
      validationErrors = identifiers.map(validateArchisIdentifier).collect { case Failure(e) => e.getMessage }
      _ = if (validationErrors.nonEmpty) fail(formatInvalidArchisIdentifiers(validationErrors).mkString("\n"))
    } yield ()
  }

  private def getArchisIdentifiers(ddm: Node): Seq[String] = {
    (ddm \\ "identifier")
      .withFilter(_.attributes
        .filter(_.prefixedKey == "xsi:type")
        .filter(_.value.text == "id-type:ARCHIS-ZAAK-IDENTIFICATIE")
        .nonEmpty)
      .map(_.text)
  }

  private def validateArchisIdentifier(identifier: String): Try[Unit] = {
    if (identifier.length <= 10) Success(())
    else Try(fail(s"Archis identifier must be 10 or fewer characters long: $identifier"))
  }

  private def formatInvalidArchisIdentifiers(results: Seq[String]): Seq[String] = {
    results.zipWithIndex.map { case (msg, index) => s"(${ index + 1 }) $msg" }
  }

  def filesXmlHasDocumentElementFiles(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map(
      xml => if (xml.label != "files") fail("files.xml: document element must be 'files'"))
  }

  def filesXmlHasOnlyFiles(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map {
      files =>
        if (files.namespace == filesXmlNamespace) {
          debug("Rule filesXmlHasOnlyFiles has been checked by files.xsd")
        }
        else {
          val nonFiles = (files \ "_").filterNot(_.label == "file")
          if (nonFiles.nonEmpty) fail(s"files.xml: children of document element must only be 'file'. Found non-file elements: ${ nonFiles.mkString(", ") }")
        }
    }
  }

  def filesXmlFileElementsAllHaveFilepathAttribute(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map {
      xml =>
        val files = xml \ "file"
        if (files.exists(_.attribute("filepath").isEmpty)) fail("Not all 'file' elements have a 'filepath' attribute")
    }
  }

  def filesXmlAllFilesDescribedOnce(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map { xml =>
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
        def stringDiff[T](left: Set[T], right: Set[T]): String = {
          val set = left diff right
          if (set.isEmpty) "{}"
          else set.mkString("{", ", ", "}")
        }

        lazy val onlyInBag = stringDiff(payloadPaths, pathsInFileXml)
        lazy val onlyInFilesXml = stringDiff(pathsInFileXml, payloadPaths)

        val msg1 = if (noDuplicatesFound) ""
                   else s"   - Duplicate filepaths found: ${ duplicatePathsInFilesXml.mkString("{", ", ", "}") }\n"
        val msg2 = if (fileSetsEqual) ""
                   else "   - Filepaths in files.xml not equal to files found in data folder. Difference: " +
                     s"(only in bag: $onlyInBag, only in files.xml: $onlyInFilesXml)"

        val msg = msg1 + msg2
        fail(s"files.xml: errors in filepath-attributes:\n$msg")
      }
    }
  }

  def filesXmlAllFilesHaveFormat(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map { xml =>
      val files = xml \ "file"
      val allFilesHaveFormat = files.forall(_.child.exists(n =>
        xml.getNamespace(n.prefix) == "http://purl.org/dc/terms/" && n.label == "format"))
      if (!allFilesHaveFormat) fail("files.xml: not all <file> elements contain a <dcterms:format>")
    }
  }

  def filesXmlFilesHaveOnlyAllowedNamespaces(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map { xml =>
      if (xml.namespace == filesXmlNamespace) {
        debug("Rule filesXmlFilesHaveOnlyAllowedNamespaces has been checked by files.xsd")
      }
      else {
        val fileChildren = xml \ "file" \ "_"
        val hasOnlyAllowedNamespaces = fileChildren.forall {
          case n: Elem => allowedFilesXmlNamespaces contains xml.getNamespace(n.prefix)
          case _ => true // Don't check non-element nodes
        }
        if (!hasOnlyAllowedNamespaces) fail("files.xml: non-dc/dcterms elements found in some file elements")
      }
    }
  }

  def optionalFileIsUtf8Decodable(f: Path)(t: TargetBag): Try[Unit] = {
    for {
      _ <- Try(assume(!f.isAbsolute, "Path to UTF-8 text file must be relative."))
      file = t.bagDir / f.toString
      _ <- if (file.exists) isValidUtf8(file.byteArray).recoverWith { case e: CharacterCodingException => fail(s"Input not valid UTF-8: ${ e.getMessage }") }
           else Success(())
    } yield ()
  }

  private def isValidUtf8(input: Array[Byte]): Try[Unit] = {
    val cs = Charset.forName("UTF-8").newDecoder
    Try {
      cs.decode(ByteBuffer.wrap(input))
    }
  }
}
