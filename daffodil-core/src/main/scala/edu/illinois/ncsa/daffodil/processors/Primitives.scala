package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import java.math.BigInteger
import java.text.{ ParseException, ParsePosition }
import java.util.regex.Pattern
import java.nio.{ CharBuffer, ByteBuffer }
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ YesNo, LengthKind, ByteOrder, LengthUnits }
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.util.Misc.bytes2Hex
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import com.ibm.icu.text.{ NumberFormat, DecimalFormat }
import edu.illinois.ncsa.daffodil.grammar.Terminal
import scala.util.parsing.input.{ Reader }
import java.sql.Timestamp
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextTrimKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextStringJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextCalendarJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextBooleanJustification
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }

abstract class PrimParser(gram: Gram, contextArg: SchemaComponent)
  extends DaffodilParser(contextArg) {

  def toBriefXML(depthLimit: Int = -1): String = {
    "<" + gram.name + "/>"
  }

  override def toString = toBriefXML()

}

case class ElementBegin(e: ElementBase) extends Terminal(e, true) {

  val isHidden = e.isHidden

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ElementBegin name='" + e.name + "'/>"
    }

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val currentElement = Infoset.newElement(e, isHidden)

      log(LogLevel.Debug, "currentElement = %s", currentElement)
      val priorElement = start.infoset
      priorElement.addElement(currentElement)
      log(LogLevel.Debug, "priorElement = %s", priorElement)
      val postState = start.withParent(currentElement)
      postState
    }
  }

  def unparser = new DummyUnparser(e)
  //  def unparser: Unparser = new Unparser(e) {
  //    override def toString = "<" + e.name + ">"
  //
  //    /**
  //     * Changes the state to refer to the next element in the infoset as the element to unparse.
  //     */
  //    def unparse(start: UState): UState = {
  //      val nextElement = {
  //        //
  //        // TODO FIXME: THis can't be correct. The elementBegin shouldn't be writing out element contents.
  //        // That should happen in content unparsers. This unparser should just set things up so content
  //        // unparsers see the correct element to take the contents of. Which really means just changing the 
  //        // parent pointer in the UState.
  //        //
  //        // TODO: should not try/catch - should return a failed UState on error
  //        try { //if content contains elements
  //          if (!start.childIndexStack.isEmpty) {
  //            if (start.childPos != 1) { //if not first child, write unparsed result of previous child to outputStream
  //              //              val encoder = e.knownEncodingEncoder
  //              //              start.outStream.setEncoder(encoder)
  //              start.outStream.write()
  //            }
  //            start.currentElement.getContent().get(start.childPos.asInstanceOf[Int] - 1).asInstanceOf[org.jdom.Element]
  //          } else {
  //            //            val encoder = e.knownEncodingEncoder
  //            //            start.outStream.setEncoder(encoder)
  //            start.currentElement.getContent().get(0).asInstanceOf[org.jdom.Element]
  //          }
  //        } catch {
  //          case u: UnsuppressableException => throw u
  //          case e: Exception => start.currentElement //if content is text
  //        }
  //      }
  //
  //      start.withCurrent(nextElement).moveOverByOneElement
  //    }
  //  }
}

// FIXME: ComplexElementBeginPattern
// Must be refactored to share code with ElementBegin, because enhancements made 
// there must otherwise be reproduced here. Ditto for ComplexElementEndPattern and EndElement.
// Furthermore, the whole ElementBegin/ElementEnd pairs idiom wants to be turned into
// A runtime parser combinator for diagnostics/debugging reasons, so that a try-finally can be used
// to insure we remove failed elements from the infoset, etc. See StmtEval, which is this kind of
// an 'enclosing' combinator I'm describing. 

//case class ComplexElementBeginPattern(e: ElementBase)
//  extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern)
//  with WithParseErrorThrowing {
//  Assert.invariant(e.isComplexType)
//
//  val charset = e.knownEncodingCharset
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "<" + e.name + " dfdl:lengthKind='pattern'>"
//    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.
//    val pattern = e.lengthPattern
//
//    /**
//     * ElementBegin just adds the element we are constructing to the infoset and changes
//     * the state to be referring to this new element as what we're parsing data into.
//     */
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//      withLoggingLevel(LogLevel.Info) {
//        val eName = e.toString()
//
//        log(LogLevel.Debug, "ComplexElementBeginPattern - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3)))
//        log(LogLevel.Debug, "ComplexElementBeginPattern - %s - Parsing pattern at bit position: %s", eName, start.bitPos))
//
//        val in = start.inStream
//
//        val reader = in.getCharReader(charset, start.bitPos)
//
//        val d = new delimsearch.DelimParser(e)
//
//        var result: delimsearch.DelimParseResultult = new delimsearch.DelimParseResultult
//
//        result = d.parseInputPatterned(pattern, reader)
//
//        val postState1 =
//          if (result.isSuccess) {
//            val endBitPos = start.bitPos + result.numBits
//            log(LogLevel.Debug, "Parsed: %s", result.field))
//            log(LogLevel.Debug, "Ended at bit position %s", endBitPos))
//
//            // LETS NOT DO IT THIS WAY
//            // val limitedInStream = in.withLimit(start.bitPos, endBitPos)
//            // Since we've created a new sub-stream with just the limited part of data in it,
//            // don't forget to have the position in it start at zero.
//            // start withEndBitLimit (endBitPos) withInStream (limitedInStream) withPos (0, 0)
//            // INSTEAD, LETS GET THE END LIMIT RIGHT
//            start withEndBitLimit (endBitPos)
//          } else { return PE(start, "%s: No match found!", this.toString()) }
//        val currentElement = Infoset.newElement(e)
//        log(LogLevel.Debug, "currentElement = %s", currentElement))
//        val priorElement = postState1.infoset
//        priorElement.addElement(currentElement)
//        log(LogLevel.Debug, "priorElement = %s", priorElement))
//        val postState2 = postState1 withParent (currentElement)
//        postState2
//      }
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}

abstract class ElementEndBase(e: ElementBase) extends Terminal(e, true) {
  def toPrettyString = "</" + e.name + prettyStringModifier + ">"
  def prettyStringModifier: String

  def move(pstate: PState): PState // implement for different kinds of "moving over to next thing"

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ElementEnd name='" + e.name + "'/>"
    }

    override def toString = toPrettyString

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = {
      val currentElement = start.parentElement
      // Assert.invariant(currentElement.getName() != "_document_" )
      val priorElement = currentElement.parent
      log(LogLevel.Debug, "priorElement = %s", priorElement)
      val postState = move(start.withParent(priorElement))
      postState
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "</" + e.name + ">"

    /**
     * Changes state to refer to parent element of the current one.
     */
    def unparse(start: UState): UState = {
      val postState = {
        if (start.currentElement.getName != start.rootName) {
          val parent = start.currentElement.getParentElement()
          val state = start.withCurrent(parent)
          state
        } else {
          start
        }
      }
      postState
    }
  }
}

case class ElementEnd(e: ElementBase) extends ElementEndBase(e) {
  def move(pstate: PState) = pstate.moveOverByOneElement
  def prettyStringModifier = ""
}

case class ElementEndNoRep(e: ElementBase) extends ElementEndBase(e) {
  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want 
  // the group 'thinking' that there's a prior sibling inside the group and placing a 
  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
  def move(pstate: PState) = pstate.moveOverOneElementChildOnly
  def prettyStringModifier = "(NoRep)"
}

//case class ComplexElementEndPattern(e: ElementBase) extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern) {
//  // TODO: Should this be more generic; is there a way to detect state from the current element to tell us if it's time
//  //       to pop the input stack?
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "</" + e.name + " dfdl:lengthKind='pattern'>"
//
//    /**
//     * ElementEnd just moves back to the parent element of the current one.
//     */
//    def parse(start: PState): PState = {
//      val currentElement = start.parentElement
//      log(LogLevel.Debug, "currentElement = %s", currentElement))
//      var priorElement = currentElement.parent
//      log(LogLevel.Debug, "priorElement = %s", priorElement))
//      val postState = start.withParent(priorElement).moveOverByOneElement.withLastInStream()
//      postState
//    }
//  }
//
//  def unparser: Unparser = new DummyUnparser(e)
//}

///**
// * The I/O layer should be written to use Java's NIO Channels, and Direct ByteBuffers for file I/O. This is the
// * fastest stuff in the Java stack.
// *
// * The basic design for known-length parsing
// * is to do a bounds check on whether we have enough buffer space to accomplish the reading of data
// * within the buffer. If not, repositioning/refreshing the buffer. If so, then we want to issue a
// * single block read per element which both reads and converts to the right character encoding, or
// * directly to the right type (float, long, double, etc) when the result type is a primitive.
// *
// * For types with more complexity, (binary dates for example) then the previous would still be done
// * followed by a conversion of some sort.
// */
//
//case class StringFixedLengthInBytes(e: ElementBase, nBytes: Long)
//  extends Terminal(e, true)
//  with TextReader
//  with Padded
//  with WithParseErrorThrowing {
//
//  val charset = e.knownEncodingCharset
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//
//    override def toBriefXML(depthLimit: Int = -1) = {
//      "<StringFixedLengthInBytesParser length='" + nBytes + "'/>"
//    }
//
//    override def toString = toBriefXML()
//
//    val codepointWidth = e.knownEncodingWidthInBits
//    Assert.invariant(codepointWidth != -1)
//
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//      // withLoggingLevel(LogLevel.Info) 
//      {
//
//        log(LogLevel.Debug, "StringFixedLengthInBytes - Parsing starting at bit position: %s", start.bitPos))
//
//        // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInBytes - not byte aligned.") }
//
//        val in = start.inStream
//
//        val bytePos = (start.bitPos >> 3).toInt
//
//        val decoder = charset.newDecoder()
//
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//        try {
//          //
//          // nBytes can be a little bit longer that the decoder actually consumes
//          // because some characters are less than a byte wide (7-bit ascii packed variant)
//          //
//          // Note: This code goes around the DFDLCharReader layer because that layer doesn't provide
//          // us a way to limit the number of bytes. (Perhaps that could be changed so this could use it?)
//          //
//
//          val reader = getReader(charset, start.bitPos, start)
//          val bytes = in.getBytes(start.bitPos, nBytes.toInt)
//          decoder.reset()
//          val cb = decoder.decode(ByteBuffer.wrap(bytes))
//          val result = cb.toString()
//          val endBitPos = start.bitPos + (result.length * codepointWidth) // handles 7-bit or wider chars
//          log(LogLevel.Debug, "Parsed: %s", result))
//          log(LogLevel.Debug, "Ended at bit position %s", endBitPos))
//          val endCharPos = start.charPos + result.length
//          val currentElement = start.parentElement
//          val trimmedResult = d.removePadding(result, justificationTrim, padChar)
//          // Assert.invariant(currentElement.getName != "_document_")
//          // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
//          // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
//          currentElement.setDataValue(trimmedResult)
//          // 
//          // if the number of bytes was a multiple of the codepointWidth then 
//          // we will have parsed all the bytes, so the endBitPos and endCharPos 
//          // are synchronized still. 
//          // 
//          val postState =
//            if ((endBitPos - start.bitPos) == (8 * nBytes)) {
//              start.withPos(endBitPos, endCharPos, Some(reader))
//            } else {
//              Assert.invariant((endBitPos - start.bitPos) < (8 * nBytes))
//              start.withPos(endBitPos, -1, None)
//              // -1 means a subsequent primitive will have to construct
//              // a new reader at said bitPosition              
//            }
//          return postState
//        } catch {
//          case m: java.nio.charset.MalformedInputException => { return PE(start, "StringFixedLengthInBytes - Malformed data. Could not decode into %s characters.", charset.name()) }
//          case e: IndexOutOfBoundsException => { return PE(start, "StringFixedLengthInBytes - Insufficient Bits in field: IndexOutOfBounds: \n%s", e.getMessage()) }
//          case u: UnsuppressableException => throw u
//          // case e: Exception => { return PE(start, "StringFixedLengthInBytes - Exception: \n%s", e.getMessage()) }
//        }
//      }
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    override def toString = "StringFixedLengthInBytesUnparser(" + nBytes + ")"
//
//    def unparse(start: UState): UState = {
//      // setLoggingLevel(LogLevel.Info)
//
//      val data = start.currentElement.getText
//
//      val encoder = charset.newEncoder()
//      start.outStream.setEncoder(encoder)
//      start.outStream.fillCharBuffer(data)
//
//      log(LogLevel.Debug, "Unparsed: " + start.outStream.getData))
//      start
//    }
//  }
//}
//
//case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long)
//  extends Terminal(e, true)
//  with WithParseErrorThrowing
//  with Padded {
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "StringFixedLengthInBytesVariableWidthCharactersParser(" + nBytes + ")"
//
//    val charset = e.knownEncodingCharset
//
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//      // withLoggingLevel(LogLevel.Info) 
//      {
//
//        log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos))
//
//        // no longer require alignment (some charsets aren't aligned.)
//        // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - not byte aligned.") }
//
//        val in = start.inStream
//
//        val decoder = charset.newDecoder()
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//        try {
//          val bytes = in.getBytes(start.bitPos, nBytes.toInt)
//          decoder.reset()
//          val cb = decoder.decode(ByteBuffer.wrap(bytes))
//          val result = cb.toString
//          val endBitPos = start.bitPos + (nBytes.toInt * 8)
//          log(LogLevel.Debug, "Parsed: %s", result))
//          log(LogLevel.Debug, "Ended at bit position %s", endBitPos))
//          val endCharPos = start.charPos + result.length
//          val currentElement = start.parentElement
//          // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
//          // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
//          currentElement.setDataValue(d.removePadding(result, justificationTrim, padChar))
//          val postState = start.withPos(endBitPos, -1, None) // charPos = -1 tells subsequent calls to construct
//          return postState
//        } catch {
//          case e: IndexOutOfBoundsException => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - Insufficient Bits in field. IndexOutOfBoundsException: Index was: %s", e.getMessage()) }
//          case u: UnsuppressableException => throw u
//          case e: Exception => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - Exception: \n%s", e.getMessage()) }
//        }
//        start
//      }
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}
//
//case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, nChars: Long)
//  extends Terminal(e, true)
//  with WithParseErrorThrowing with TextReader with Padded {
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "StringFixedLengthInVariableWidthCharactersParser(" + nChars + ")"
//
//    val charset = e.knownEncodingCharset
//
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//
//      log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos))
//
//      // no longer require alignment (some encodings aren't whole bytes)
//      // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInVariableWidthCharacters - not byte aligned.") }
//
//      log(LogLevel.Debug, "Retrieving reader"))
//
//      val reader = getReader(charset, start.bitPos, start)
//
//      //        val byteReader = in.byteReader.atPos(bytePos)
//      //
//      //        val reader = byteReader.charReader(decoder.charset().name())
//
//      val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//      val result = d.parseInputNCharacters(nChars, reader, justificationTrim, padChar)
//
//      result match {
//        case _: DelimParseFailure =>
//          return PE(start, "Parse failed to find exactly %s characters.", nChars)
//        case s: DelimParseSuccess => {
//
//          val parsedField = s.field
//          val parsedBits = s.numBits
//          val endBitPos = start.bitPos + parsedBits
//
//          log(LogLevel.Debug, "Parsed: %s", parsedField))
//          log(LogLevel.Debug, "Ended at bit position: %s", endBitPos))
//
//          //val endCharPos = start.charPos + nChars
//          //val endCharPos = reader.characterPos + nChars
//          val endCharPos = if (start.charPos == -1) nChars else start.charPos + nChars
//          val currentElement = start.parentElement
//          currentElement.setDataValue(parsedField)
//          //        val postState = start.withPos(endBitPos, endCharPos)
//          val postState = start.withPos(endBitPos, endCharPos, Some(s.next))
//          postState
//
//          //      //setLoggingLevel(LogLevel.Info)
//          //
//          //      log(LogLevel.Debug, this.toString() + " - Parsing starting at bit position: " + start.bitPos))
//          //
//          //      // We don't know the width of the characters, so decode as much data as possible.
//          //      // We will truncate as necessary later.
//          //      val in = start.inStream
//          //      val (endBitPos, _) = in.fillCharBufferMixedData(cbuf, start.bitPos, decoder)
//          //
//          //      val result = cbuf.toString
//          //
//          //      if (result == null) { return start.failed(this.toString() + " - Result was null!") }
//          //
//          //      val finalResult = result.substring(0, nChars.toInt) // Truncate
//          //      val finalResultBytes = finalResult.getBytes(decoder.charset()).length
//          //      val finalBitPos = 8 * finalResultBytes + start.bitPos
//          //
//          //      if (finalResult.length < nChars) { return start.failed(this.toString() + " - Result(" + finalResult.length + ") was not at least nChars (" + nChars + ") long.") }
//          //
//          //      log(LogLevel.Debug, "Parsed: " + finalResult))
//          //      log(LogLevel.Debug, "Ended at bit position " + finalBitPos))
//          //      val endCharPos = start.charPos + nChars
//          //      val currentElement = start.parentElement
//          //      // Assert.invariant(currentElement.getName != "_document_")
//          //      // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
//          //      // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
//          //      currentElement.addContent(new org.jdom.Text(finalResult))
//          //      val postState = start.withPos(finalBitPos, endCharPos)
//          //      postState
//        }
//      }
//    }
//  }
//
//  def unparser: Unparser = new DummyUnparser(e)
//}
//
//case class StringDelimitedEndOfData(eArg: ElementBase)
//  extends Terminal(eArg, true)
//  with WithParseErrorThrowing with TextReader with Padded {
//  def e = eArg
//  lazy val es = e.escapeScheme
//  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
//  lazy val tm = e.allTerminatingMarkup
//  lazy val cname = toString
//
//  val charset = e.knownEncodingCharset
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
//
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//
//      val eName = e.toString()
//
//      // We must feed variable context out of one evaluation and into the next.
//      // So that the resulting variable map has the updated status of all evaluated variables.
//      var vars = start.variableMap
//      val delimsRaw = e.allTerminatingMarkup.map {
//        x =>
//          {
//            val R(res, newVMap) = x.evaluate(start.parentElement, vars, start)
//            vars = newVMap
//            res
//          }
//      }
//      val delimsCooked1 = delimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, e).cooked })
//      val delimsCooked = delimsCooked1.flatten
//      //System.err.println("startCharPos: " + start.charPos)
//      val postEvalState = start.withVariables(vars)
//
//      if (delimsCooked.filter(x => x == "%WSP*;").length > 0) {
//        // We cannot detect this error until expressions have been evaluated!
//        log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited", eName))
//        e.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited!")
//      }
//
//      log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, delimsCooked, delimsCooked.length))
//
//      val bytePos = (postEvalState.bitPos >> 3).toInt
//      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
//      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos))
//
//      val reader = getReader(charset, postEvalState.bitPos, postEvalState)
//
//      val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//      val result = try {
//
//        if (esObj.escapeSchemeKind == EscapeSchemeKind.Block) {
//          //          result = d.parseInputEscapeBlock(Set.empty[String], delimsCooked.toSet, reader,
//          //            esObj.escapeBlockStart, esObj.escapeBlockEnd, esObj.escapeEscapeCharacter)
//          d.parseInputEscapeBlock(Set.empty[String], delimsCooked.toSet, reader,
//            esObj.escapeBlockStart, esObj.escapeBlockEnd, esObj.escapeEscapeCharacter, justificationTrim, padChar)
//        } else if (esObj.escapeSchemeKind == EscapeSchemeKind.Character) {
//          d.parseInputEscapeCharacter(Set.empty[String], delimsCooked.toSet, reader,
//            esObj.escapeCharacter, esObj.escapeEscapeCharacter, justificationTrim, padChar)
//        } else {
//          d.parseInput(Set.empty[String], delimsCooked.toSet, reader, justificationTrim, padChar)
//        }
//      } catch {
//        case mie: MalformedInputException =>
//          throw new ParseError(e, Some(postEvalState), "Malformed input, length: %s", mie.getInputLength())
//      }
//
//      result match {
//        case f: DelimParseFailure =>
//          //System.err.println("StringDelimitedEndOfData_END: " + new Timestamp(System.currentTimeMillis()));
//          return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
//        case s: DelimParseSuccess => {
//          //System.err.println("StringDelimitedEndOfData_END: " + new Timestamp(System.currentTimeMillis()));
//          val field = s.get
//          val numBits = s.numBits
//          log(LogLevel.Debug, "%s - Parsed: %s Parsed Bytes: %s (bits %s)", eName, field, numBits / 8, numBits))
//          //System.err.println(postEvalState.charPos)
//          //val endCharPos = reader.characterPos + field.length()
//          val endCharPos = if (postEvalState.charPos == -1) s.numCharsRead else postEvalState.charPos + s.numCharsRead
//          //val endCharPos = postEvalState.charPos + field.length()
//          val endBitPos = postEvalState.bitPos + numBits
//          val currentElement = postEvalState.parentElement
//          currentElement.setDataValue(field)
//          //return postEvalState.withPos(endBitPos, endCharPos)
//          return postEvalState.withPos(endBitPos, endCharPos, Some(s.next))
//        }
//      }
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
//
//    def unparse(start: UState): UState =
//      // withLoggingLevel(LogLevel.Info) 
//      {
//        val data = start.currentElement.getText
//
//        val encoder = charset.newEncoder()
//        start.outStream.setEncoder(encoder)
//        start.outStream.fillCharBuffer(data)
//        log(LogLevel.Debug, "Unparsed: " + start.outStream.getData))
//        start
//      }
//  }
//}
//
//case class StringPatternMatched(e: ElementBase)
//  extends Terminal(e, true)
//  with WithParseErrorThrowing with TextReader with Padded {
//
//  val charset = e.knownEncodingCharset
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "StringPatternMatched"
//    val pattern = e.lengthPattern
//
//    // TODO: Add parameter for changing CharBuffer size
//
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//      // withLoggingLevel(LogLevel.Info) 
//      {
//        val eName = e.toString()
//
//        log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3)))
//        log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at bit position: %s", eName, start.bitPos))
//
//        // some encodings aren't whole bytes.
//        // if (start.bitPos % 8 != 0) { return PE(start, "StringPatternMatched - not byte aligned.") }
//
//        val bytePos = (start.bitPos >> 3).toInt
//
//        log(LogLevel.Debug, "Retrieving reader"))
//
//        val reader = getReader(charset, start.bitPos, start)
//
//        //        val byteReader = in.byteReader.atPos(bytePos)
//        //        val reader = byteReader.charReader(decoder.charset().name())
//
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//        val result = d.parseInputPatterned(pattern, reader)
//
//        val postState = result match {
//          case _: DelimParseFailure => { PE(start, "%s: No match found!", this.toString()) }
//          case s: DelimParseSuccess => {
//            val endBitPos = start.bitPos + s.numBits
//            log(LogLevel.Debug, "StringPatternMatched - Parsed: %s", s.field))
//            log(LogLevel.Debug, "StringPatternMatched - Ended at bit position %s", endBitPos))
//            // val endCharPos = start.charPos + result.field.length()
//            //val endCharPos = reader.characterPos + result.field.length()
//            val endCharPos = if (start.charPos == -1) s.field.length() else start.charPos + s.field.length()
//            val currentElement = start.parentElement
//            currentElement.setDataValue(s.field)
//            //start.withPos(endBitPos, endCharPos)
//            start.withPos(endBitPos, endCharPos, Some(s.next))
//          }
//
//        }
//        //      log(LogLevel.Debug, "Parsing starting at bit position: " + start.bitPos))
//        //      val in = start.inStream
//        //      var bitOffset = 0L
//        //
//        //      val (result, endBitPos, theState) = in.fillCharBufferWithPatternMatch(cbuf, start.bitPos, decoder, pattern)
//        //
//        //      val postState = theState match {
//        //        case SearchResult.NoMatch => start.failed(this.toString() + ": No match found!")
//        //        case SearchResult.PartialMatch => start.failed(this.toString() + ": Partial match found!")
//        //        case SearchResult.FullMatch => {
//        //          log(LogLevel.Debug, "Parsed: " + result))
//        //          log(LogLevel.Debug, "Ended at bit position " + endBitPos))
//        //          val endCharPos = start.charPos + result.length()
//        //          val currentElement = start.parentElement
//        //          currentElement.setDataValue(result)
//        //          start.withPos(endBitPos, endCharPos)
//        //        }
//        //      }
//        //      postState
//        postState
//      }
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}

abstract class ConvertTextNumberPrim[S](e: ElementBase, guard: Boolean)
  extends Terminal(e, guard) {

  protected def getNum(s: Number): S
  protected val GramName: String
  protected val GramDescription: String

  override def toString = "to(xs:" + GramName + ")"

  protected def numFormat: NumberFormat
  protected def isInt: Boolean

  protected def isInvalidRange(n: java.lang.Number): Boolean

  protected def getStringFormat(n: S): String

  def compare(num1: Number, num2: Number) = {
    val bd1 = new java.math.BigDecimal(num1.toString)
    val bd2 = new java.math.BigDecimal(num2.toString)
    bd1.compareTo(bd2)
  }

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "to(xs:" + GramName + ")"

    def parse(start: PState): PState = {

      val node = start.parentElement
      var str = node.dataValue

      Assert.invariant(str != null) // worst case it should be empty string. But not null.
      val resultState = try {
        // Strip leading + (sign) since the DecimalFormat can't handle it
        if (str.length > 0 && str.charAt(0) == '+') {
          // TODO: There needs to be a way to restore '+' in the unparse, but that will be in the format field
          str = str.substring(1)
        }
        if (str == "") return PE(start, "Convert to %s (for xs:%s): Cannot parse number from empty string", GramDescription, GramName)

        // Need to use Decimal Format to parse even though this is an Integral number
        val df = numFormat
        val pos = new ParsePosition(0)
        val num = try {
          df.parse(str, pos)
        } catch {
          case u: UnsuppressableException => throw u
          case e: Exception =>
            return PE(start, "Convert to %s (for xs:%s): Parse of '%s' threw exception %s",
              GramDescription, GramName, str, e)

        }
        if (isInvalidRange(num)) {
          return PE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
            GramDescription, GramName, str, num)
        }

        // Verify that what was parsed was what was passed exactly in byte count.  
        // Use pos to verify all characters consumed & check for errors!
        if (pos.getIndex != str.length) {
          return PE(start, "Convert to %s (for xs:%s): Unable to parse '%s' (using up all characters).",
            GramDescription, GramName, str)
        }

        // convert to proper type
        val asNumber = getNum(num)

        // This isn't entirely correct, Number doesn't implement comparator
        // must perform this check using BigDecimal.compareTo, found when implementing
        // facets. 01/29/2013
        //        // Verify no digits lost (the number was correctly transcribed)
        //        if (isInt && asNumber.asInstanceOf[Number] != num) {
        //          // Transcription error
        //          return PE(start, "Convert to %s (for xs:%s): Invalid data: '%s' parsed into %s, which converted into %s.",
        //            GramDescription, GramName, str, num, asNumber)
        //        }

        // Verify no digits lost (the number was correctly transcribed)
        if (isInt && (compare(asNumber.asInstanceOf[Number], num) != 0)) {
          // Transcription error
          return PE(start, "Convert to %s (for xs:%s): Invalid data: '%s' parsed into %s, which converted into %s.",
            GramDescription, GramName, str, num, asNumber)
        }

        // The following change was made because of the issues with the float
        // adding a position of precision to the Number object.  At some point we
        // will want to revert this back to the actual type but this is a quick fix
        // for the issues we were having with the 0.003 vs 0.0030 error in test_DelimProp_05
        //
        //node.setText(asNumber.toString)
        val result = getStringFormat(asNumber)
        node.setDataValue(result)

        start
      } // catch { case e: Exception => start.failed("Failed to convert %s to an xs:%s" + GramName) }

      resultState
    }
  }

  // TODO: consolidate duplicate code
  def unparser: Unparser = new Unparser(e) {
    override def toString = "to(xs:" + GramName + ")"

    /*
      * Converts data to number format, returns unparse exception if data cannot be converted to given format.
      */
    def unparse(start: UState): UState = {
      // TODO: OK to get from infoset?
      var str = start.currentElement.getText //gets data from element being unparsed
      Assert.invariant(str != null) // worst case it should be empty string. But not null.
      if (str == "") return UE(start, "Convert to %s (for xs:%s): Cannot unparse number from empty string", GramDescription, GramName)

      //make sure data can parse to appropriate type
      val df = numFormat
      val pos = new ParsePosition(0)
      val num = try {
        df.parse(str, pos)
      } catch {
        case u: UnsuppressableException => throw u
        case e: Exception =>
          return UE(start, "Convert to %s (for xs:%s): Unparse of '%s' threw exception %s",
            GramDescription, GramName, str, e)
      }
      if (isInvalidRange(num)) {
        return UE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
          GramDescription, GramName, str, num)
      }

      // Verify that what was unparsed was what was passed exactly in byte count.  
      // Use pos to verify all characters consumed & check for errors!
      if (pos.getIndex != str.length) {
        return UE(start, "Convert to %s (for xs:%s): Unable to unparse '%s' (using up all characters).",
          GramDescription, GramName, str)
      }

      // convert to proper type
      val asNumber = getNum(num)

      // Verify no digits lost (the number was correctly transcribed)
      if (isInt && asNumber.asInstanceOf[Number] != num) {
        // Transcription error
        return UE(start, "Convert to %s (for xs:%s): Invalid data: '%s' unparsed into %s, which converted into %s.",
          GramDescription, GramName, str, num, asNumber)
      }

      // TODO: Restore leading '+' sign and leading/trailing 0's, etc. (Need to overwrite number with old formatting in CharBuffer
      //      log(LogLevel.Debug, "Adding text number " + asNumber.toString))

      start
    }
  }
}

abstract class ConvertTextIntegerNumberPrim[T](e: ElementBase, g: Boolean)
  extends ConvertTextNumberPrim[T](e, g) {
  override def numFormat = NumberFormat.getIntegerInstance()
  override def isInt = true

  protected override def getStringFormat(n: T): String = n.toString()

  protected def isInvalidRange(n: java.lang.Number): Boolean = {
    //
    // Note: Scala has no class analogous to java.lang.Number. There's no common 
    // base class above its number types (as there isn't above the Java *primitive* number types.)
    //
    // We're being handed here a java 'boxed' number type, and those have common parent Number.
    //
    // println("number's actual type is: " + n.getClass.getName)
    //
    // This method only for things that fit in range of a Long. (i.e., not unbounded size Integer, and not unsignedLong
    // Nevertheless, if invalid data much too long for the real numeric type is what is found in the data
    // then a java BigInteger (or maybe even BigDecimal might get passed here.
    // 
    // The only thing we can check is whether there is conversion to a long available.
    // e.g., like this: Assert.invariant(n.isInstanceOf[{ def longValue : Long}]) 
    // But that's eliminated by erasure, so we'll just do without.
    //
    if (n == null) false // we tolerate null here. Something else figures out the error.
    else {
      val l = n.longValue
      isInvalidRange(l)
    }
  }
  def min: Long
  def max: Long
  private def isInvalidRange(l: Long) = {
    l < min || l > max
  }
}

abstract class ConvertTextFloatingPointNumberPrim[T](e: ElementBase, g: Boolean)
  extends ConvertTextNumberPrim[T](e, g) {
  override def numFormat = NumberFormat.getNumberInstance() // .getScientificInstance() Note: scientific doesn't allow commas as grouping separators.
  override def isInt = false
  protected override def getStringFormat(n: T): String = {
    //val trailingZeroes = """0*(?!<[1-9])$"""
    val trailingZeroes = """(?<=[1-9])(0*)$""".r
    val trailingZeroesBeforeExponent = """(?<=[1-9])(0*?)(?=E.*)""".r

    val nAsStr = n.toString()

    if (nAsStr.contains("E") || nAsStr.contains("e")) {
      // Exponent
      return trailingZeroesBeforeExponent.replaceAllIn(nAsStr, "")
    } else {
      return trailingZeroes.replaceAllIn(nAsStr, "")
    }

    nAsStr
  }
}

case class ConvertTextIntegerPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "integer"
  protected override val GramDescription = "Unbounded Integer"
  protected override def isInvalidRange(n: java.lang.Number): Boolean = false
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertTextLongPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "long"
  protected override val GramDescription = "Long Integer"
  val min = Long.MinValue
  val max = Long.MaxValue
}

case class ConvertTextIntPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "int"
  protected override val GramDescription = "Integer"
  val min = Int.MinValue.toLong
  val max = Int.MaxValue.toLong
}

case class ConvertTextShortPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "short"
  protected override val GramDescription = "Short Integer"
  val min = Short.MinValue.toLong
  val max = Short.MaxValue.toLong
}

case class ConvertTextBytePrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Byte](e, true) {
  protected override def getNum(num: Number) = num.byteValue
  protected override val GramName = "byte"
  protected override val GramDescription = "Byte"
  val min = Byte.MinValue.toLong
  val max = Byte.MaxValue.toLong
}

case class ConvertTextUnsignedLongPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "unsignedLong"
  protected override val GramDescription = "Unsigned Long"
  protected override def isInvalidRange(jn: java.lang.Number) = {
    jn match {
      case n: BigInteger => {
        n.compareTo(BigInteger.ZERO) < 0 || n.compareTo(BigInteger.ONE.shiftLeft(64)) >= 0
      }
      case null => false // tolerate null. Deal with that error elsewhere.
      case _ => {
        val n = jn.longValue()
        n < 0 // note: the other side of the check is inherently ok since a Long must be smaller than an unsignedLong.
      }
    }
  }
  val min = 0.toLong
  val max = -1.toLong // unused.
}

case class ConvertTextUnsignedIntPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "unsignedInt"
  protected override val GramDescription = "Unsigned Int"
  val min = 0L
  val max = (1L << 32) - 1L
}
// TODO: Restore leading '+' sign and leading/trailing 0's
case class ConvertTextUnsignedShortPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "unsignedShort"
  protected override val GramDescription = "Unsigned Short"
  val min = 0L
  val max = (1L << 16) - 1L
}

case class ConvertTextUnsignedBytePrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "unsignedByte"
  protected override val GramDescription = "Unsigned Byte"
  val min = 0L
  val max = (1L << 8) - 1L
}

case class ConvertTextDoublePrim(e: ElementBase) extends ConvertTextFloatingPointNumberPrim[Double](e, true) {
  protected override def getNum(num: Number) = num.doubleValue
  protected override val GramName = "double"
  protected override val GramDescription = "Double"
  protected def isInvalidRange(n: java.lang.Number): Boolean = false
}

case class ConvertTextFloatPrim(e: ElementBase) extends ConvertTextFloatingPointNumberPrim[Float](e, true) {
  protected override def getNum(num: Number) = num.floatValue
  protected override val GramName = "float"
  protected override val GramDescription = "Float"
  protected def isInvalidRange(n: java.lang.Number): Boolean = {
    if (n == null) return false // tolerate null here. We catch that error elsewhere.
    val d = n.doubleValue()
    if (d.isNaN) false
    else (d < Float.MinValue) || d > Float.MaxValue
  }

}

abstract class Primitive(e: AnnotatedSchemaComponent, guard: Boolean = false)
  extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
  def parser: DaffodilParser = DummyParser(e)
  def unparser: Unparser = DummyUnparser(e)

}

abstract class DelimParserBase(e: Term, guard: Boolean) extends Terminal(e, guard) {
  override def toString = "DelimParserBase[" + name + "]"
  val dp = new DFDLDelimParserStatic(e.knownEncodingStringBitLengthFunction)
}

abstract class ZonedTextNumberPrim(e: ElementBase, guard: Boolean) extends Terminal(e, guard) {
  def parser: DaffodilParser = new PrimParser(this, e) {
    def parse(start: PState): PState = {
      Assert.notYetImplemented()
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}
case class ZonedTextBytePrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextShortPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextIntPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextLongPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)

trait RuntimeExplicitLengthMixin[T] {
  self: Terminal =>
  def e: ElementBase

  // get at compile time, not runtime.
  val lUnits = e.lengthUnits

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getBitLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes * toBits)
  }
  def getLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes)
  }
}

trait KnownLengthInBitsMixin[T] {
  self: BinaryNumberBase[T] =>
  def len: Long
  def getBitLength(s: PState) = (s, len) // already in bits, so no multiply by 8 for this one.
}

trait RuntimeExplicitByteOrderMixin[T] {
  self: BinaryNumberBase[T] =>
  def e: ElementBase
  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    val R(byteOrderAsAny, newVMap) = e.byteOrder.evaluate(s.parentElement, s.variableMap, s)
    val dfdlByteOrderEnum = ByteOrder(byteOrderAsAny.toString, e)
    val byteOrder = dfdlByteOrderEnum match {
      case ByteOrder.BigEndian => java.nio.ByteOrder.BIG_ENDIAN
      case ByteOrder.LittleEndian => java.nio.ByteOrder.LITTLE_ENDIAN
    }
    val start = s.withVariables(newVMap)
    (start, byteOrder)
  }
}

trait SignedNumberMixin[T] {
  self: BinaryNumberBase[T] =>
  def convertValue(n: BigInt, msb: Int): T = {
    val signed = n.testBit(msb - 1) match { // msb is zero-based bit counting
      case true => n - (BigInt(1) << msb)
      case false => n
    }
    signed.asInstanceOf[T]
  }
}

trait UnsignedNumberMixin[T] {
  self: BinaryNumberBase[T] =>
  def convertValue(n: BigInt, msb: Int): T = n.asInstanceOf[T]
}

// TODO: Double Conversion as a Sign-Trait

abstract class BinaryNumberBase[T](val e: ElementBase) extends Terminal(e, true) {
  lazy val primName = e.primType.name

  lazy val staticByteOrderString = e.byteOrder.constantAsString
  lazy val staticByteOrder = ByteOrder(staticByteOrderString, context)

  lazy val (staticJByteOrder, label) = staticByteOrder match {
    case ByteOrder.BigEndian => (java.nio.ByteOrder.BIG_ENDIAN, "BE")
    case ByteOrder.LittleEndian => (java.nio.ByteOrder.LITTLE_ENDIAN, "LE")
  }

  //def getNum(t: Number): BigInt
  protected def getBitLength(s: PState): (PState, Long)
  protected def getByteOrder(s: PState): (PState, java.nio.ByteOrder)
  protected def convertValue(n: BigInt, msb: Int): T
  override def toString = "binary(xs:" + primName + ", " + label + ")"
  val gram = this

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1, GramName.length) }

  def parser = new PrimParser(this, e) {
    override def toString = gram.toString

    def parse(start0: PState): PState = {
      try {
        val (start1, nBits) = getBitLength(start0)
        val (start, bo) = getByteOrder(start1)
        val (value, newPos) = start.inStream.getBitSequence(start.bitPos, nBits, bo)
        //if (GramName == "hexBinary") {
        //  val bytes = value.asInstanceOf[Array[Byte]]
        //  var asString: StringBuilder = new StringBuilder()
        //  for (i <- 0 until bytes.length) {
        //    val byte = String.format("%02X", bytes(i).asInstanceOf[java.lang.Byte])
        //    asString.append(byte)
        //  }
        //  start.parentElement.setDataValue(asString.toString())
        //} else
        val convertedValue: T = convertValue(value, nBits toInt)
        start.parentElement.setDataValue(convertedValue.toString)
        start.withPos(newPos, -1, None)
      } catch {
        case e: IndexOutOfBoundsException => { return PE(start0, "BinaryNumber - Insufficient Bits for xs:%s : IndexOutOfBounds: \n%s", primName, e.getMessage()) }
        case u: UnsuppressableException => throw u
        case e: Exception => { return PE(start0, "BinaryNumber - Exception: \n%s", e.getStackTraceString) }
      }
    }
  }

  def unparser = DummyUnparser
}

class UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with UnsignedNumberMixin[T] {
}

class UnsignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] with UnsignedNumberMixin[T] {
}

class SignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with SignedNumberMixin[T] {
}

class SignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] with SignedNumberMixin[T] {
}

// Not needed. No runtime-determined lengths for binary floats.
//class FloatingPointRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
//  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with FloatingPointMixin[T] {
//}

class FloatKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Float](e)
  with RuntimeExplicitByteOrderMixin[Float]
  with KnownLengthInBitsMixin[Float] {

  final def convertValue(n: BigInt, ignored_msb: Int): Float = {
    val nWith33rdBit = n | (BigInt(1) << 33) // make sure we have 5 bytes here. Then we'll ignore 5th byte.
    val ba = nWith33rdBit.toByteArray
    val bb = java.nio.ByteBuffer.wrap(ba)
    val res = ba.length match {
      case 5 => bb.getFloat(1)
      case _ => Assert.invariantFailed("byte array should be 5 long")
    }
    res
  }
}

class DoubleKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Double](e)
  with RuntimeExplicitByteOrderMixin[Double]
  with KnownLengthInBitsMixin[Double] {

  final def convertValue(n: BigInt, ignored_msb: Int): Double = {
    val nWith65thBit = n | (BigInt(1) << 65) // make sure we have 9 bytes of bigint here. Then we'll ignore the 9th byte.
    val ba = nWith65thBit.toByteArray
    val bb = java.nio.ByteBuffer.wrap(ba)
    val res = ba.length match {
      case 9 => bb.getDouble(1) // ignore first byte.
      case _ => Assert.invariantFailed("byte array should be 9 long")
    }
    res
  }
}

case class PackedIntPrim(e: ElementBase) extends Primitive(e, false)
case class BCDIntPrim(e: ElementBase) extends Primitive(e, false)

//abstract class StaticDelimiter(kindString: String, delim: String, e: Term, guard: Boolean = true)
//  extends StaticText(delim, e, kindString, guard)
//
//abstract class StaticText(delim: String, e: Term, kindString: String, guard: Boolean = true)
//  extends Terminal(e, guard)
//  with WithParseErrorThrowing with TextReader {
//
//  val charset = e.knownEncodingCharset
//
//  lazy val es = e.escapeScheme
//  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
//
//  val term = e.asInstanceOf[Term]
//  lazy val staticTexts = delim.split("\\s").toList
//
//  val staticTextsCooked: Queue[String] = new Queue
//
//  staticTexts.foreach(x => staticTextsCooked.enqueue(EntityReplacer.replaceAll(x)))
//
//  lazy val delimsRaw = e.allTerminatingMarkup.map { _.constantAsString }
//  lazy val delimsCooked1 = delimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, e).cooked })
//  lazy val delimsCooked = delimsCooked1.flatten
//
//  // Here we expect that remoteDelims shall be defined as those delimiters who are not
//  // also defined locally.  That is to say that local should win over remote.
//  lazy val remoteDelims = delimsCooked.toSet.diff(staticTextsCooked.toSet)
//  
//  // here we define the parsers so that they are pre-compiled/generated
//  lazy val delimParser = new DFDLDelimParserStatic(e.knownEncodingStringBitLengthFunction)
//  lazy val (pInputDelimiterParser, pIsLocalDelimParser) = 
//    delimParser.generateInputDelimiterParsers(staticTextsCooked.toSet, remoteDelims)
//    
//  def parseMethod(input: Reader[Char]): DelimParseResult = {
//    val result: DelimParseResult = delimParser.parseInputDelimiter(pInputDelimiterParser, pIsLocalDelimParser, input)
//    result
//  }
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//
//    override def toBriefXML(depthLimit: Int = -1) = {
//      "<" + kindString + ">" + delim + " " + delimsRaw + "</" + kindString + ">"
//    }
//
//    e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' not supported.")
//
//    Assert.invariant(delim != "") // shouldn't be here at all in this case.
//    override def toString = kindString + "('" + delim + "')" //  with terminating markup: " + term.prettyTerminatingMarkup + ")"
//
//    //    val delimsRaw = e.allTerminatingMarkup.map { _.constantAsString }
//    //    val delimsCooked1 = delimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, e).cooked })
//    //    val delimsCooked = delimsCooked1.flatten
//    //
//    //    // Here we expect that remoteDelims shall be defined as those delimiters who are not
//    //    // also defined locally.  That is to say that local should win over remote.
//    //    val remoteDelims = delimsCooked.toSet.diff(staticTextsCooked.toSet)
//    //System.err.println("startCharPos: " + start.charPos)
//
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//      // withLoggingLevel(LogLevel.Debug) 
//      {
//        val eName = e.toString()
//
//        // We must feed variable context out of one evaluation and into the next.
//        // So that the resulting variable map has the updated status of all evaluated variables.
//        var vars = start.variableMap
//
//        val postEvalState = start.withVariables(vars)
//
//        log(LogLevel.Debug, "%s - Parsing delimiter at byte position: %s", eName, (postEvalState.bitPos >> 3)))
//        log(LogLevel.Debug, "%s - Parsing delimiter at bit position: %s", eName, postEvalState.bitPos))
//
//        log(LogLevel.Debug, "%s - Looking for local(%s) not remote (%s).", eName, staticTextsCooked.toSet, remoteDelims))
//
//        val bytePos = (postEvalState.bitPos >> 3).toInt
//
//        log(LogLevel.Debug, "Retrieving reader state."))
//        val reader = getReader(charset, start.bitPos, postEvalState)
//
//        //val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//        // Well they may not be delimiters, but the logic is the same as for a 
//        // set of static delimiters.
//        //val result = d.parseInputDelimiter(staticTextsCooked.toSet, remoteDelims, reader)
//        //val result: DelimParseResult = delimParser.parseInputDelimiter(pInputDelimiterParser, pIsLocalDelimParser, reader)
//        val result = parseMethod(reader)
//        
//        log(LogLevel.Debug, "%s - %s - DelimParseResultult: %s", this.toString(), eName, result))
//
//        result match {
//          case _: DelimParseFailure => {
//            log(LogLevel.Debug, "%s - %s: Delimiter not found!", this.toString(), eName))
//            return PE(start, "%s - %s: Delimiter not found!", this.toString(), eName)
//          }
//          case s: DelimParseSuccess if (s.delimiterLoc == DelimiterLocation.Remote) => {
//            log(LogLevel.Debug, "%s - %s: Remote delimiter found instead of local!", this.toString(), eName))
//            return PE(start, "%s - %s: Remote delimiter found instead of local!", this.toString(), eName)
//          }
//          case s: DelimParseSuccess =>
//            {
//              val numBits = e.knownEncodingStringBitLengthFunction(s.delimiter)
//              val endCharPos = if (postEvalState.charPos == -1) s.delimiter.length else postEvalState.charPos + s.delimiter.length()
//              val endBitPosDelim = numBits + postEvalState.bitPos
//
//              log(LogLevel.Debug, "%s - Found %s", eName, s.delimiter))
//              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPosDelim >> 3)))
//              log(LogLevel.Debug, "%s - Ended at bit position %s", eName, endBitPosDelim))
//
//              return postEvalState.withPos(endBitPosDelim, endCharPos, Some(s.next))
//            }
//            postEvalState
//        }
//      }
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    val t = e.asInstanceOf[Term]
//    override def toString = "StaticText('" + delim + "' with terminating markup: " + t.prettyTerminatingMarkup + ")"
//    // setLoggingLevel(LogLevel.Info)
//    e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' is not supported.")
//    Assert.invariant(delim != "") //shouldn't be here at all in this case
//
//    def unparse(start: UState): UState = {
//      val encoder = e.knownEncodingCharset.newEncoder()
//      start.outStream.setEncoder(encoder)
//      start.outStream.fillCharBuffer(unparserDelim)
//      log(LogLevel.Debug, "Unparsed: " + start.outStream.getData))
//      start
//    }
//  }
//
//  def unparserDelim: String
//}
//
//class DynamicDelimiter(delimExpr: CompiledExpression, e: Term, guard: Boolean = true) extends Primitive(e, guard)
//
////case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e)
//case class StaticInitiator(e: Term) extends StaticDelimiter("Init", e.initiator.constantAsString, e) {
//  Assert.invariant(e.hasInitiator)
//  lazy val unparserDelim = e.initiator.constantAsString.split("""\s""").head
//}
////case class StaticTerminator(e : Term) extends StaticDelimiter(e.terminator.constantAsString, e)
//case class StaticTerminator(e: Term) extends StaticDelimiter("Term", e.terminator.constantAsString, e) {
//  Assert.invariant(e.hasTerminator)
//  lazy val unparserDelim = e.terminator.constantAsString.split("""\s""").head
//}
//case class DynamicInitiator(e: Term) extends DynamicDelimiter(e.initiator, e)
//case class DynamicTerminator(e: Term) extends DynamicDelimiter(e.terminator, e)
//
//case class StaticSeparator(s: Sequence, t: Term) extends StaticDelimiter("Sep", s.separator.constantAsString, t) {
//  Assert.invariant(s.hasSeparator)
//  lazy val unparserDelim = s.separator.constantAsString.split("""\s""").head
//}
//case class DynamicSeparator(s: Sequence, t: Term) extends DynamicDelimiter(s.separator, t)

case class StartChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: DaffodilParser = new PrimParser(this, ct.element) {
    override def toString = "StartChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "StartChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }
}

case class StartSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {

  def parser: DaffodilParser = new PrimParser(this, sq) {
    override def toString = "StartSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser(sq) {
    override def toString = "StartSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

case class Nada(sc: Term) extends Terminal(sc, true) {
  override def isEmpty = false
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"

  def parser: DaffodilParser = new PrimParser(this, sc) {
    override def toString = "Nada"

    def parse(start: PState): PState = start
  }

  def unparser: Unparser = new Unparser(sc) {
    override def toString = "Nada"

    def unparse(start: UState): UState = start
  }
}

case class GroupPosGreaterThan(groupPos: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: DaffodilParser = new PrimParser(this, term) {
    override def toString = "GroupPosGreaterThan(" + groupPos + ")"

    def parse(start: PState): PState = {
      val res = if (start.groupPos > groupPos) {
        start.withDiscriminator(true)
      } else {
        PE(start, "Group position not greater than (%s)", groupPos)
      }
      res
    }
  }

  def unparser: Unparser = new Unparser(term) {
    override def toString = "GroupPosGreaterThan(" + groupPos + ")"

    def unparse(start: UState): UState = {
      val res = if (start.groupPos > groupPos) {
        start.withDiscriminator(true)
      } else {
        UE(start, "Group position not greater than (%s)", groupPos)
      }
      res
    }
  }
}

case class ChildPosGreaterThan(childPos: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: DaffodilParser = new PrimParser(this, term) {
    override def toString = "ChildPosGreaterThan(" + childPos + ")"

    def parse(start: PState): PState = {
      val res = if (start.childPos > childPos) {
        start.withDiscriminator(true)
      } else {
        PE(start, "Child position not greater than (%s)", childPos)
      }
      res
    }
  }

  def unparser: Unparser = new Unparser(term) {
    override def toString = "ChildPosGreaterThan(" + childPos + ")"

    def unparse(start: UState): UState = {
      val res = if (start.childPos > childPos) {
        start.withDiscriminator(true)
      } else {
        UE(start, "Child position not greater than (%s)", childPos)
      }
      res
    }
  }
}

case class ArrayPosGreaterThan(arrayPos: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: DaffodilParser = new PrimParser(this, term) {
    override def toString = "ArrayPosGreaterThan(" + arrayPos + ")"

    def parse(start: PState): PState = {
      val res = try {
        if (start.arrayPos > arrayPos) {
          start.withDiscriminator(true)
        } else {
          PE(start, "Array position not greater than (%s)", arrayPos)
        }
      } catch { case e => PE(start, "No array position") }
      res
    }
  }

  def unparser: Unparser = new Unparser(term) {
    override def toString = "ArrayPosGreaterThan(" + arrayPos + ")"

    def unparse(start: UState): UState = {
      val res = try {
        if (start.arrayPos > arrayPos) {
          start.withDiscriminator(true)
        } else {
          UE(start, "Array position not greater than (%s)", arrayPos)
        }
      } catch { case e => UE(start, "No array position") }
      res
    }
  }
}

case class EndChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: DaffodilParser = new PrimParser(this, ct.element) {
    override def toString = "EndChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "EndChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }
}

case class EndSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {

  def parser: DaffodilParser = new PrimParser(this, sq) {
    override def toString = "EndSequence"

    def parse(start: PState): PState = {
      // When we end a sequence group, we have created a group child in the parent
      // so we advance that index. 
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
      postState
    }
  }

  def unparser: Unparser = new Unparser(sq) {
    override def toString = "EndSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
      postState
    }
  }
}

case class StartArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "StartArray"

    def parse(start: PState): PState = {
      val postState1 = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      val postState2 = postState1.withOccursCountStack(Compiler.occursCountMax :: postState1.occursCountStack)
      postState2
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "StartArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      postState
    }
  }
}

case class EndArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "EndArray"

    def parse(start: PState): PState = {
      val postState1 = start.withArrayIndexStack(start.arrayIndexStack.tail)
      val postState2 = postState1.withOccursCountStack(postState1.occursCountStack.tail)
      postState2
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "EndArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(start.arrayIndexStack.tail)
      postState
    }
  }
}

case class NoValue(e: GlobalElementDecl, guard: Boolean = true) extends Primitive(e, guard)

case class SaveInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

case class SetEmptyInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

case class RestoreInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

//case class Value(e: SchemaComponent, guard: Boolean = true) extends Primitive(e, guard) 

case class NotStopValue(e: ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class StopValue(e: ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class TheDefaultValue(e: ElementBase) extends Primitive(e, e.isDefaultable)

//case class LiteralNilExplicitLengthInBytes(e: ElementBase)
//  extends LiteralNilInBytesBase(e, "LiteralNilExplicit") {
//
//  val expr = e.length
//  val exprText = expr.prettyExpr
//
//  final def computeLength(start: PState) = {
//    val R(nBytesAsAny, newVMap) = expr.evaluate(start.parentElement, start.variableMap, start)
//    val nBytes = nBytesAsAny.toString().toLong //nBytesAsAny.asInstanceOf[Long]
//    (nBytes, newVMap)
//  }
//
//}
//
//case class LiteralNilKnownLengthInBytes(e: ElementBase, lengthInBytes: Long)
//  extends LiteralNilInBytesBase(e, "LiteralNilKnown") {
//
//  final def computeLength(start: PState) = {
//    (lengthInBytes, start.variableMap)
//  }
//
//}
//
//abstract class LiteralNilInBytesBase(e: ElementBase, label: String)
//  extends StaticText(e.nilValue, e, label, e.isNillable)
//  with Padded {
//
//  protected def computeLength(start: PState): (Long, VariableMap)
//
//  // We are to assume that we can always read nBytes
//  // a failure to read nBytes is a failure period.
//
//  lazy val unparserDelim = Assert.notYetImplemented()
//
//  override def parser = new PrimParser(this, e) {
//
//    override def toBriefXML(depthLimit: Int = -1): String = {
//      "<" + name + " nilValue='" + e.nilValue + "'/>"
//    }
//
//    val isEmptyAllowed = e.nilValue.contains("%ES;")
//    val eName = e.toString()
//    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
//    val charsetName = charset.name()
//
//    def parse(start: PState): PState = {
//      //      withLoggingLevel(LogLevel.Debug) 
//      {
//
//        // TODO: What if someone passes in nBytes = 0 for Explicit length, is this legal?
//
//        val (nBytes: Long, newVMap: VariableMap) = computeLength(start)
//        val postEvalState = start.withVariables(newVMap)
//        log(LogLevel.Debug, "Explicit length %s", nBytes))
//
//        //val postEvalState = start //start.withVariables(vars)
//
//        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length))
//        val in = postEvalState.inStream
//
//        val bytePos = (postEvalState.bitPos >> 3).toInt
//        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
//        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos))
//
//        // some encodings aren't whole bytes
//        // if (postEvalState.bitPos % 8 != 0) { return PE(postEvalState, "LiteralNilPattern - not byte aligned.") }
//
//        val decoder = charset.newDecoder()
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//        try {
//          val reader = in.getCharReader(charset, postEvalState.bitPos)
//          val bytes = in.getBytes(postEvalState.bitPos, nBytes.toInt)
//          val cb = decoder.decode(ByteBuffer.wrap(bytes))
//          val result = cb.toString
//          val trimmedResult = d.removePadding(result, justificationTrim, padChar)
//          val endBitPos = postEvalState.bitPos + (nBytes.toInt * 8)
//          val endCharPos = if (postEvalState.charPos == -1) result.length() else postEvalState.charPos + result.length()
//
//          // We have a field, is it empty?
//          val isFieldEmpty = trimmedResult.length == 0 //result.length() == 0
//
//          if (isFieldEmpty && isEmptyAllowed) {
//            // Valid!
//            postEvalState.parentElement.makeNil()
//            return postEvalState // Empty, no need to advance
//          } else if (isFieldEmpty && !isEmptyAllowed) {
//            // Fail!
//            return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
//          } else if (d.isFieldDfdlLiteral(trimmedResult, nilValuesCooked.toSet)) {
//            // Contains a nilValue, Success!
//            postEvalState.parentElement.makeNil()
//
//            log(LogLevel.Debug, "%s - Found %s", eName, trimmedResult))
//            log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3)))
//            log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos))
//
//            return postEvalState.withPos(endBitPos, endCharPos, Some(reader)) // Need to advance past found nilValue
//          } else {
//            // Fail!
//            return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
//          }
//        } catch {
//          case e: IndexOutOfBoundsException => {
//            // In this case, we failed to get the bytes
//            if (isEmptyAllowed) {
//              // Valid!
//              postEvalState.parentElement.makeNil()
//              return postEvalState // Empty, no need to advance
//            } else {
//              return PE(postEvalState, "%s - Insufficient Bytes in field; required %s", name, nBytes)
//            }
//          }
//          case u: UnsuppressableException => throw u
//          case e: Exception => { return PE(postEvalState, "%s - Exception: \n%s", name, e.getMessage()) }
//        }
//      }
//    }
//
//  }
//
//  override def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}
//
//case class LiteralNilExplicitLengthInChars(e: ElementBase)
//  extends StaticText(e.nilValue, e, "LiteralNilExplicit", e.isNillable)
//  with Padded {
//  // We are to assume that we can always read nChars
//  // a failure to read nChars is a failure period.
//
//  // TODO: LiteralNilExplicitLengthInChars really is a variation of LiteralNilPattern
//  lazy val unparserDelim = Assert.notYetImplemented()
//
//  override def parser = new PrimParser(this, e) {
//
//    override def toBriefXML(depthLimit: Int = -1): String = {
//      "<" + name + " nilValue='" + e.nilValue + "'/>"
//    }
//
//    val isEmptyAllowed = e.nilValue.contains("%ES;")
//    val eName = e.toString()
//    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
//    val charsetName = charset.name()
//    val expr = e.length
//    val exprText = expr.prettyExpr
//
//    def parse(start: PState): PState = {
//      // withLoggingLevel(LogLevel.Info) 
//      {
//
//        //val postEvalState = start //start.withVariables(vars)
//
//        val R(nCharsAsAny, newVMap) = expr.evaluate(start.parentElement, start.variableMap, start)
//        val nChars = nCharsAsAny.asInstanceOf[String] //nBytesAsAny.asInstanceOf[Long]
//        val postEvalState = start.withVariables(newVMap)
//        log(LogLevel.Debug, "Explicit length %s", nChars))
//
//        val pattern = "(?s)^.{%s}".format(nChars)
//
//        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length))
//
//        val bytePos = (postEvalState.bitPos >> 3).toInt
//        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
//        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos))
//
//        // Don't check this here. This can vary by encoding.
//        //if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }
//
//        log(LogLevel.Debug, "Retrieving reader state."))
//        val reader = getReader(charset, start.bitPos, start)
//
//        if (nChars == 0 && isEmptyAllowed) {
//          log(LogLevel.Debug, "%s - explicit length of 0 and %ES; found as nilValue.", eName))
//          postEvalState.parentElement.makeNil()
//          return postEvalState // Empty, no need to advance
//        }
//
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//        val result = d.parseInputPatterned(pattern, reader)
//
//        result match {
//          case _: DelimParseFailure =>
//            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
//          case s: DelimParseSuccess => {
//            // We have a field, is it empty?
//            val field = d.removePadding(s.field, justificationTrim, padChar)
//            val isFieldEmpty = field.length() == 0
//
//            if (isFieldEmpty && isEmptyAllowed) {
//              // Valid!
//              start.parentElement.makeNil()
//              return postEvalState // Empty, no need to advance
//            } else if (isFieldEmpty && !isEmptyAllowed) {
//              // Fail!
//              return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
//            } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
//              // Contains a nilValue, Success!
//              start.parentElement.makeNil()
//
//              val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)
//              val endCharPos =
//                if (postEvalState.charPos == -1) s.field.length
//                else postEvalState.charPos + s.field.length
//              val endBitPos = numBits + start.bitPos
//
//              log(LogLevel.Debug, "%s - Found %s", eName, s.field))
//              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3)))
//              log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos))
//
//              return postEvalState.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
//            } else {
//              // Fail!
//              return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
//            }
//          }
//        }
//      }
//    }
//  }
//
//  override def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//
//}
//
//case class LiteralNilExplicit(e: ElementBase, nUnits: Long)
//  extends StaticText(e.nilValue, e, "LiteralNilExplicit", e.isNillable)
//  with Padded {
//  lazy val unparserDelim = Assert.notYetImplemented()
//  //val stParser = super.parser
//
//  override def parser = new PrimParser(this, e) {
//
//    override def toBriefXML(depthLimit: Int = -1): String = {
//      "<" + name + " nilValue='" + e.nilValue + "'/>"
//    }
//
//    val pattern = e.lengthPattern
//
//    val isEmptyAllowed = e.nilValue.contains("%ES;")
//    val eName = e.toString()
//    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
//    val charsetName = charset.name()
//
//    def parse(start: PState): PState = {
//      // withLoggingLevel(LogLevel.Info) 
//      {
//
//        val postEvalState = start //start.withVariables(vars)
//
//        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length))
//
//        val bytePos = (postEvalState.bitPos >> 3).toInt
//        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
//        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos))
//
//        if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }
//
//        log(LogLevel.Debug, "Retrieving reader state."))
//        val reader = getReader(charset, start.bitPos, start)
//
//        //        val byteReader = in.byteReader.atPos(bytePos)
//        //        val reader = byteReader.charReader(decoder.charset().name())
//
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//        val result = d.parseInputPatterned(pattern, reader)
//
//        result match {
//          case _: DelimParseFailure =>
//            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
//          case s: DelimParseSuccess => {
//            // We have a field, is it empty?
//            val field = d.removePadding(s.field, justificationTrim, padChar)
//            val isFieldEmpty = field.length() == 0
//
//            if (isFieldEmpty && isEmptyAllowed) {
//              // Valid!
//              start.parentElement.makeNil()
//              return postEvalState // Empty, no need to advance
//            } else if (isFieldEmpty && !isEmptyAllowed) {
//              // Fail!
//              return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
//            } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
//              // Contains a nilValue, Success!
//              start.parentElement.makeNil()
//
//              val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)
//              //val endCharPos = start.charPos + result.field.length()
//              val endCharPos =
//                if (postEvalState.charPos == -1) s.field.length
//                else postEvalState.charPos + s.field.length
//              val endBitPos = numBits + start.bitPos
//
//              log(LogLevel.Debug, "%s - Found %s", eName, s.field))
//              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3)))
//              log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos))
//
//              //return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
//              return postEvalState.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
//            } else {
//              // Fail!
//              return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
//            }
//          }
//        }
//      }
//    }
//  }
//
//  override def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}
//
//case class LiteralNilPattern(e: ElementBase)
//  extends StaticText(e.nilValue, e, "LiteralNilPattern", e.isNillable)
//  with Padded {
//  lazy val unparserDelim = Assert.notYetImplemented()
//  //val stParser = super.parser
//
//  override def parser = new PrimParser(this, e) {
//
//    override def toBriefXML(depthLimit: Int = -1): String = {
//      "<" + name + " nilValue='" + e.nilValue + "'/>"
//    }
//
//    val pattern = e.lengthPattern
//
//    val isEmptyAllowed = e.nilValue.contains("%ES;")
//    val eName = e.toString()
//    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
//    val charsetName = charset.name()
//
//    def parse(start: PState): PState = {
//      // withLoggingLevel(LogLevel.Info) 
//      {
//
//        val postEvalState = start //start.withVariables(vars)
//
//        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length))
//
//        val bytePos = (postEvalState.bitPos >> 3).toInt
//        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
//        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos))
//
//        if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }
//
//        log(LogLevel.Debug, "Retrieving reader state."))
//        val reader = getReader(charset, start.bitPos, start)
//
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//        val result = d.parseInputPatterned(pattern, reader)
//
//        result match {
//          case _: DelimParseFailure =>
//            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
//          case s: DelimParseSuccess => {
//            // We have a field, is it empty?
//            val field = d.removePadding(s.field, justificationTrim, padChar)
//            val isFieldEmpty = field.length() == 0
//
//            if (isFieldEmpty && isEmptyAllowed) {
//              // Valid!
//              start.parentElement.makeNil()
//              return postEvalState // Empty, no need to advance
//            } else if (isFieldEmpty && !isEmptyAllowed) {
//              // Fail!
//              return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
//            } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
//              // Contains a nilValue, Success!
//              start.parentElement.makeNil()
//
//              val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)
//
//              val endCharPos =
//                if (postEvalState.charPos == -1) s.field.length
//                else postEvalState.charPos + s.field.length
//              val endBitPos = numBits + start.bitPos
//
//              log(LogLevel.Debug, "%s - Found %s", eName, s.field))
//              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3)))
//              log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos))
//
//              return postEvalState.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
//            } else {
//              // Fail!
//              return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
//            }
//          }
//        }
//      }
//    }
//  }
//
//  override def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}
//
//case class LiteralNilDelimitedOrEndOfData(e: ElementBase)
//  extends StaticText(e.nilValue, e, "LiteralNilDelimitedOrEndOfData", e.isNillable)
//  with Padded {
//  lazy val unparserDelim = Assert.notYetImplemented()
//
//  val stParser = super.parser
//
//  override def parser = new PrimParser(this, e) {
//    override def toString = "LiteralNilDelimitedOrEndOfData(" + e.nilValue + ")"
//
//    def parse(start: PState): PState = {
//      // withLoggingLevel(LogLevel.Debug) 
//      {
//        val eName = e.toString()
//
//        // TODO: Why are we even doing this here?  LiteralNils don't care about delimiters
//        // and LiteralNils don't have expressions! I think we can get rid of this variable
//        // mapping stuff.
//        //
//        // We must feed variable context out of one evaluation and into the next.
//        // So that the resulting variable map has the updated status of all evaluated variables.
//        var vars = start.variableMap
//        val delimsRaw = e.allTerminatingMarkup.map {
//          x =>
//            {
//              val R(res, newVMap) = x.evaluate(start.parentElement, vars, start)
//              vars = newVMap
//              res
//            }
//        }
//        val delimsCooked1 = delimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, e).cooked })
//        val delimsCooked = delimsCooked1.flatten
//        //val nilValuesCooked1 = delimsRaw.map(raw => { new ListOfStringValueAsLiteral(e.nilValue, e).cooked })
//        val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked //nilValuesCooked1.flatten
//        val postEvalState = start.withVariables(vars)
//
//        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, delimsCooked, delimsCooked.length))
//
//        val bytePos = (postEvalState.bitPos >> 3).toInt
//        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
//        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos))
//
//        // Don't check alignment this way. This is encoding specific. (Some encodings not byte aligned)
//        // if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilDelimitedOrEndOfData - not byte aligned.") }
//
//        log(LogLevel.Debug, "Retrieving reader state."))
//        val reader1 = getReader(charset, start.bitPos, start)
//        //        val byteReader = in.byteReader.atPos(bytePos)
//        //        val reader = byteReader.charReader(decoder.charset().name())
//
//        // 1. Parse up until terminating Markup
//        // 2. Compare resultant field to nilValue(s)
//        //		Same, success
//        //		Diff, fail
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//        val result =
//          if (esObj.escapeSchemeKind == EscapeSchemeKind.Block) {
//            //          result = d.parseInputEscapeBlock(Set.empty[String], delimsCooked.toSet, reader,
//            //            esObj.escapeBlockStart, esObj.escapeBlockEnd, esObj.escapeEscapeCharacter)
//            d.parseInputEscapeBlock(Set.empty[String], delimsCooked.toSet, reader1,
//              esObj.escapeBlockStart, esObj.escapeBlockEnd, esObj.escapeEscapeCharacter, justificationTrim, padChar)
//          } else if (esObj.escapeSchemeKind == EscapeSchemeKind.Character) {
//            d.parseInputEscapeCharacter(Set.empty[String], delimsCooked.toSet, reader1,
//              esObj.escapeCharacter, esObj.escapeEscapeCharacter, justificationTrim, padChar)
//          } else {
//            d.parseInput(Set.empty[String], delimsCooked.toSet, reader1, justificationTrim, padChar)
//          }
//
//        result match {
//          case f: DelimParseFailure =>
//            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
//          case s: DelimParseSuccess => {
//            // We have a field, is it empty?
//            val field = d.removePadding(s.field, justificationTrim, padChar)
//            val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars
//            val isEmptyAllowed = e.nilValue.contains("%ES;") // TODO: move outside parser
//            if (isFieldEmpty && !isEmptyAllowed) {
//              // Fail!
//              return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
//            } else if ((isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any. 
//              d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) { // Not empty, but matches.
//              // Contains a nilValue, Success!
//              start.parentElement.makeNil()
//
//              val numBits = s.numBits
//              //val endCharPos = start.charPos + result.field.length()
//              val endCharPos = if (postEvalState.charPos == -1) s.numCharsRead else postEvalState.charPos + s.numCharsRead
//              val endBitPos = numBits + start.bitPos
//
//              log(LogLevel.Debug, "%s - Found %s", eName, s.field))
//              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3)))
//              log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos))
//
//              //return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
//              return postEvalState.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
//            } else {
//              // Fail!
//              return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
//            }
//          }
//        }
//      }
//    }
//  }
//
//  override def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}
//
//case class LogicalNilValue(e: ElementBase) extends Primitive(e, e.isNillable)

// As soon as you turn these on (by removing the false and putting the real guard), then schemas all need to have
// these properties in them, which is inconvenient until we have multi-file schema support and format references.
case class LeadingSkipRegion(e: Term) extends Terminal(e, e.leadingSkip > 0) {
  e.schemaDefinitionUnless(e.leadingSkip < Compiler.maxSkipLength, "Property leadingSkip %s is larger than limit %s", e.leadingSkip, Compiler.maxSkipLength)

  def parser: DaffodilParser = new PrimParser(this, e) {

    def parse(pstate: PState) = {

      val newBitPos = 8 * (pstate.bytePos + e.leadingSkip)
      pstate.withPos(newBitPos, -1, None)
    }

    override def toString = "leadingSkip(" + e.leadingSkip + ")"
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(ustate: UState) = {

      Assert.notYetImplemented()
    }
  }
}

case class AlignmentFill(e: Term) extends Primitive(e, false) // e.alignment != AlignmentType.Implicit)

case class TrailingSkipRegion(e: Term) extends Primitive(e, false) // e.trailingSkip > 0)

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e: ElementBase) extends Primitive(e, false)

abstract class NewVariableInstanceBase(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends Terminal(decl, true) {
  val (uri, localName) = XMLUtils.QName(decl.xml, stmt.ref, decl.schemaDocument)
  val expName = XMLUtils.expandedQName(uri, localName)
}

case class NewVariableInstanceStart(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new PrimParser(this, decl) {
    stmt.notYetImplemented("newVariableInstance")
    def parse(pstate: PState) = {
      stmt.notYetImplemented("newVariableInstance")
    }
  }

  def unparser: Unparser = Assert.notYetImplemented()

}

case class NewVariableInstanceEnd(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new PrimParser(this, decl) {
    stmt.notYetImplemented("newVariableInstance")
    def parse(pstate: PState) = stmt.notYetImplemented("newVariableInstance")
  }

  def unparser: Unparser = Assert.notYetImplemented()
}

abstract class AssertPatternBase(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends Terminal(decl, true)
  with WithParseErrorThrowing
  with TextReader {

  val eName = decl.prettyName
  val testPattern = stmt.testTxt
  val csName = decl.knownEncodingCharset.name()
  val charset = decl.knownEncodingCharset

  def parser: DaffodilParser
  def unparser: Unparser

}

case class AssertPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssert)
  extends AssertPatternBase(decl, stmt) {

  val kindString = "AssertPatternPrim"

  def parser: DaffodilParser = new PrimParser(this, decl) {

    override def toBriefXML(depthLimit: Int = -1) = {
      "<" + kindString + ">" + testPattern + "</" + kindString + ">"
    }

    def parse(start: PState): PState =
      // withLoggingLevel(LogLevel.Info) 
      {
        withParseErrorThrowing(start) {
          val lastState = start // .withLastState
          val bytePos = (lastState.bitPos >> 3).toInt
          log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, lastState.bitPos)
          log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

          log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

          if (lastState.bitPos % 8 != 0) {
            return PE(lastState, "%s - not byte aligned.", eName)
          }

          log(LogLevel.Debug, "Retrieving reader")

          val reader = getReader(charset, start.bitPos, lastState)

          val d = new DelimParser(decl.knownEncodingStringBitLengthFunction)

          val result = d.parseInputPatterned(testPattern, reader)

          val postState = result match {
            case s: DelimParseSuccess => {
              val endBitPos = lastState.bitPos + s.numBits
              log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
              start
            }
            case _ => {
              log(LogLevel.Debug, "Assert Pattern fail for testPattern %s", testPattern)
              val diag = new AssertionFailed(decl, start, stmt.message)
              start.failed(diag)
            }
          }
          postState
        }
      }
  }

  def unparser: Unparser = new Unparser(decl) {

    def unparse(start: UState): UState = {
      start
    }
  }
}

case class DiscriminatorPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends AssertPatternBase(decl, stmt) {

  val kindString = "DiscriminatorPatternPrim"

  def parser: DaffodilParser = new PrimParser(this, decl) {

    override def toBriefXML(depthLimit: Int = -1) = {
      "<" + kindString + ">" + testPattern + "</" + kindString + ">"
    }

    def parse(start: PState): PState =
      // withLoggingLevel(LogLevel.Info) 
      {
        withParseErrorThrowing(start) {
          val lastState = start // .withLastState
          val bytePos = (lastState.bitPos >> 3).toInt
          log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, lastState.bitPos)
          log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

          log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

          if (lastState.bitPos % 8 != 0) {
            return PE(lastState, "%s - not byte aligned.", eName)
          }

          log(LogLevel.Debug, "Retrieving reader")

          val reader = getReader(charset, start.bitPos, lastState)

          val d = new DelimParser(decl.knownEncodingStringBitLengthFunction)

          val result = d.parseInputPatterned(testPattern, reader)

          // Only want to set the discriminator if it is true
          // we do not want to modify it unless it's true
          if (result.isSuccess) { return start.withDiscriminator(true) }
          val diag = new AssertionFailed(decl, start, stmt.message)
          start.failed(diag)
        }
      }
  }

  def unparser: Unparser = new Unparser(decl) {

    def unparse(start: UState): UState = {
      start
    }
  }
}

abstract class AssertBase(
  decl: AnnotatedSchemaComponent,
  exprTextArg: String,
  msg: String,
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String)
  extends ExpressionEvaluatorBase(decl) {

  override val baseName = assertKindName
  override lazy val expandedTypeName = XMLUtils.XSD_BOOLEAN
  lazy val exprText = exprTextArg

  def unparser = DummyUnparser

  def parser: DaffodilParser = new ExpressionEvaluationParser(this, decl) {

    def parse(start: PState): PState =
      // withLoggingLevel(LogLevel.Info) 
      {
        withParseErrorThrowing(start) {
          log(LogLevel.Debug, "This is %s", toString)
          val R(res, newVMap) = eval(start)
          val testResult = res.asInstanceOf[Boolean]
          val postState = start.withVariables(newVMap)
          if (testResult) {
            postState.withDiscriminator(discrim)
          } else {
            // The assertion failed. Prepare a failure message etc. in case backtracking ultimately fails from here.
            val diag = new AssertionFailed(decl, postState, msg)
            postState.failed(diag)
          }
        }
      }
  }
}

abstract class AssertBooleanPrimBase(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase,
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String) extends AssertBase(decl, stmt.testTxt, stmt.message, discrim, assertKindName)

case class AssertBooleanPrim(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, false, "assert") {
}

case class DiscriminatorBooleanPrim(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, true, "discriminator")

case class InitiatedContent(
  decl: AnnotatedSchemaComponent)
  extends AssertBase(decl,
    "{ true() }", // always true. We're just an assertion that says an initiator was found.
    "initiatedContent. This message should not be used.",
    true,
    "initiatedContent")

case class SetVariable(decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable)
  extends ExpressionEvaluatorBase(decl) {

  val baseName = "SetVariable[" + stmt.localName + "]"
  lazy val exprText = stmt.value

  lazy val expandedTypeName = stmt.defv.extType

  def parser: DaffodilParser = new SetVariableParser(this, decl, stmt)
  def unparser = DummyUnparser
}

/**
 * Refactored primitives that use expressions to put expression evaluation in one place.
 * On this base (for the primitive), and a corresponding parser base class for the
 * actual evaluation.
 *
 * That fixed a bug where a SDE wasn't being reported until the parser was run that
 * could have been reported at compilation time.
 *
 * Anything being computed that involves the dsom or grammar objects or attributes of them,
 * should be done in the grammar primitives, and NOT in the parser.
 * This is important to insure errors are captured at compilation time and
 * reported on relevant objects.
 */
abstract class ExpressionEvaluatorBase(e: AnnotatedSchemaComponent) extends Terminal(e, true) {
  override def toString = baseName + "(" + exprText + ")"

  def toBriefXML(depthLimit: Int = -1) = {
    "<" + baseName + ">" + exprText + "</" + baseName + ">"
  }

  def baseName: String
  def exprText: String
  def expandedTypeName: String

  val expressionTypeSymbol = {
    // println(expandedTypeName)
    e.expressionCompiler.convertTypeString(expandedTypeName)
  }

  val expr = e.expressionCompiler.compile(expressionTypeSymbol, exprText)
}

case class InputValueCalc(e: ElementBase)
  extends ExpressionEvaluatorBase(e) {

  val baseName = "InputValueCalc"
  lazy val Some(exprText) = e.inputValueCalcOption

  lazy val pt = e.primType
  lazy val ptn = pt.name
  lazy val expandedTypeName = XMLUtils.expandedQName(XMLUtils.XSD_NAMESPACE, ptn)

  def parser: DaffodilParser = new IVCParser(this, e)
  def unparser = DummyUnparser
}

/**
 * Common parser base class for any parser that evaluates an expression.
 */
abstract class ExpressionEvaluationParser(context: ExpressionEvaluatorBase, e: AnnotatedSchemaComponent)
  extends Parser(e) with WithParseErrorThrowing {

  def toBriefXML(depthLimit: Int = -1) = context.toBriefXML(depthLimit)

  def eval(start: PState) = {
    val currentElement = start.parentElement
    val R(res, newVMap) =
      context.expr.evaluate(currentElement, start.variableMap, start)
    // val result = res.toString // Everything in JDOM is a string!
    R(res, newVMap)
  }
}

class IVCParser(context: InputValueCalc, e: ElementBase)
  extends ExpressionEvaluationParser(context, e) {
  Assert.invariant(e.isSimpleType)

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        val currentElement = start.parentElement
        val R(res, newVMap) = eval(start)
        currentElement.setDataValue(res.toString)
        val postState = start.withVariables(newVMap) // inputValueCalc consumes nothing. Just creates a value.
        postState
      }
    }
}

class SetVariableParser(context: ExpressionEvaluatorBase, decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable)
  extends ExpressionEvaluationParser(context, decl) {

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        val R(res, newVMap) = eval(start)
        val newVMap2 = newVMap.setVariable(stmt.defv.extName, res, decl)
        val postState = start.withVariables(newVMap2)
        postState
      }
    }
}

//case class StringExplicitLengthInBytes(e: ElementBase)
//  extends Terminal(e, true)
//  with TextReader
//  with Padded
//  with WithParseErrorThrowing {
//  val expr = e.length
//  val exprText = expr.prettyExpr
//  // val maxBytes = Compiler.maxFieldContentLengthInBytes
//  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
//  var cbufSize = 0
//
//  val charset = e.knownEncodingCharset
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "StringExplicitLengthInBytesParser(" + exprText + ")"
//
//    def parse(pstate: PState): PState = withParseErrorThrowing(pstate) {
//
//      log(LogLevel.Debug, "Parsing starting at bit position: %s", pstate.bitPos))
//
//      val R(nBytesAsAny, newVMap) = expr.evaluate(pstate.parentElement, pstate.variableMap, pstate)
//      val nBytes = nBytesAsAny.asInstanceOf[Long]
//      val start = pstate.withVariables(newVMap)
//      log(LogLevel.Debug, "Explicit length %s", nBytes))
//
//      if (start.bitPos % 8 != 0) { return PE(start, "StringExplicitLengthInBytes - not byte aligned.") }
//
//      val in = start.inStream
//      val decoder = charset.newDecoder()
//      val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//      val bytePos = (start.bitPos >> 3).toInt
//
//      try {
//        val reader = getReader(charset, start.bitPos, start)
//        val bytes = in.getBytes(start.bitPos, nBytes.toInt)
//        val cb = decoder.decode(ByteBuffer.wrap(bytes))
//        val result = cb.toString
//        val endBitPos = start.bitPos + (nBytes.toInt * 8)
//        log(LogLevel.Debug, "Parsed: " + result))
//        log(LogLevel.Debug, "Ended at bit position " + endBitPos))
//        val endCharPos = start.charPos + result.length
//        val currentElement = start.parentElement
//        // Assert.invariant(currentElement.getName != "_document_")
//        // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
//        // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
//        currentElement.setDataValue(d.removePadding(result, justificationTrim, padChar))
//        // FIXME: 16-bit characters and odd number of bytes - won't work.
//        // Needs to skip last byte, and pass endCharPos -1 (refactor with StringFixedLengthInBytes)
//        val postState = start.withPos(endBitPos, endCharPos, Some(reader))
//        return postState
//      } catch {
//        case e: IndexOutOfBoundsException => { return PE(start, "StringExplicitLengthInBytesParser - Insufficient Bits in field: IndexOutOfBounds: \n%s", e.getMessage()) }
//        case u: UnsuppressableException => throw u
//        case e: Exception => { return PE(start, "StringExplicitLengthInBytesParser - Exception: \n%s", e.getStackTraceString) }
//      }
//      pstate
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    override def toString = "StringExplicitLengthInBytesUnparser(" + exprText + ")"
//    //    val encoder = e.knownEncodingEncoder
//
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}

trait TextReader extends Logging {

  /**
   * Readers are stored in the PState within the InStream object.
   */
  def getReader(charset: Charset, bitPos: Long, state: PState): DFDLCharReader = {
    // withLoggingLevel(LogLevel.Info) 
    {
      val csName = charset.name()
      log(LogLevel.Debug, "Retrieving reader at bytePos %s", bitPos >> 3)
      // Do we already have a reader in the PState?
      val res = state.inStream.getCharReader(charset, bitPos)
      res
    }
  }

}

trait Padded { self: Terminal =>
  var padChar = ""
  val eBase = self.context.asInstanceOf[ElementBase]

  val justificationTrim: TextJustificationType.Type = eBase.textTrimKind match {
    case TextTrimKind.None => TextJustificationType.None
    case TextTrimKind.PadChar if eBase.isSimpleType => {
      val theJust = eBase.primType.myPrimitiveType match {

        case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long |
          PrimType.Integer | PrimType.UInt | PrimType.UByte | PrimType.UShort |
          PrimType.ULong | PrimType.Double | PrimType.Float => {
          padChar = eBase.textNumberPadCharacter
          eBase.textNumberJustification match {
            case TextNumberJustification.Left => TextJustificationType.Left
            case TextNumberJustification.Right => TextJustificationType.Right
            case TextNumberJustification.Center => TextJustificationType.Center
          }
        }
        case PrimType.String => {
          padChar = eBase.textStringPadCharacter
          eBase.textStringJustification match {
            case TextStringJustification.Left => TextJustificationType.Left
            case TextStringJustification.Right => TextJustificationType.Right
            case TextStringJustification.Center => TextJustificationType.Center
          }
        }
        case PrimType.DateTime | PrimType.Date | PrimType.Time => {
          padChar = eBase.textCalendarPadCharacter
          eBase.textCalendarJustification match {
            case TextCalendarJustification.Left => TextJustificationType.Left
            case TextCalendarJustification.Right => TextJustificationType.Right
            case TextCalendarJustification.Center => TextJustificationType.Center
          }
        }
        case PrimType.Boolean => {
          padChar = eBase.textBooleanPadCharacter
          eBase.textBooleanJustification match {
            case TextBooleanJustification.Left => TextJustificationType.Left
            case TextBooleanJustification.Right => TextJustificationType.Right
            case TextBooleanJustification.Center => TextJustificationType.Center
          }
        }
        case _ => TextJustificationType.None
      }
      theJust
    }
    case _ => TextJustificationType.None
  }

}

