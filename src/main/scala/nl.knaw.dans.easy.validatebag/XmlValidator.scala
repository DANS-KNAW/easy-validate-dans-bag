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

import java.io.InputStream

import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import nl.knaw.dans.lib.error._
import org.xml.sax.{ ErrorHandler, SAXParseException }

import scala.collection.mutable.ListBuffer
import scala.util.Try

class XmlValidator(schema: Schema) {

  def validate(is: InputStream): Try[Unit] = {
    val errorHandler = new AccumulatingErrorHandler
    Try {
        val validator = schema.newValidator()
        validator.setErrorHandler(errorHandler)
        validator.validate(new StreamSource(is))
    }.flatMap(_ => errorHandler.errors.toList.map(x => Try[Unit] { throw x }).collectResults.map(_ => ()))
  }
}

class SilentErrorHandler extends ErrorHandler {
  override def warning(exception: SAXParseException): Unit = ()

  override def error(exception: SAXParseException): Unit = ()

  override def fatalError(exception: SAXParseException): Unit = ()
}

class AccumulatingErrorHandler extends ErrorHandler {
  val warnings = new ListBuffer[Exception]
  val errors = new ListBuffer[Exception]
  val fatals = new ListBuffer[Exception]


  override def warning(exception: SAXParseException): Unit = {
    warnings.append(exception)
  }

  override def error(exception: SAXParseException): Unit = {
    errors.append(exception)
  }

  override def fatalError(exception: SAXParseException): Unit = {
    fatals.append(exception)
  }
}

