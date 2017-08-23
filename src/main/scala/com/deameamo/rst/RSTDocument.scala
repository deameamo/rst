package com.deameamo.rst

import java.awt.Color
import java.awt.event.ActionEvent
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text.{DefaultStyledDocument, Element, Style, StyleConstants}

import com.deameamo.event.ActionEventDispatcher
import com.deameamo.util.ArrayList

import scala.collection.mutable

class RSTDocument extends DefaultStyledDocument with DocumentListener {
  
  val MINOR_PUNCS = Seq('，', '：', ',', ':')
  val MAJOR_PUNCS = Seq('。', '；', '！', '？', ';', '!', '?')
  
  var displaying = false
  
  val BASIC_STYLE = "basic"
  val PARA_STYLE = "para"
  val PLAIN_STYLE = "plain"
  val CONN_STYLE = "conn"
  val HEAD_STYLE = "head"
  val TAIL_STYLE = "tail"
  
  val plainFg: Color = Resource.getColor("plainFg")
  val headFg: Color = Resource.getColor("headFg")
  val tailFg: Color = Resource.getColor("tailFg")
  val connFg: Color = Resource.getColor("connFg")
  val nodeBg: Color = Resource.getColor("nodeBg")
  val plainBg: Color = Resource.getColor("bg")
  val nucleusHighlightBg: Color = Resource.getColor("nucleusHighlightBg")
  val satelliteHighlightBg: Color = Resource.getColor("satelliteHighlightBg")
  val undefinedHighlightBg: Color = Resource.getColor("undefinedHighlightBg")
  
  val basicStyle: Style = addStyle(BASIC_STYLE, null)
  StyleConstants.setFontSize(basicStyle, Resource.fontSize)
  StyleConstants.setFontFamily(basicStyle, Resource.fontFamily)
  
  val plainStyle: Style = addStyle(PLAIN_STYLE, basicStyle)
  StyleConstants.setForeground(plainStyle, plainFg)
  val connStyle: Style = addStyle(CONN_STYLE, basicStyle)
  StyleConstants.setForeground(connStyle, connFg)
  val headStyle: Style = addStyle(HEAD_STYLE, basicStyle)
  StyleConstants.setForeground(headStyle, headFg)
  val tailStyle: Style = addStyle(TAIL_STYLE, basicStyle)
  StyleConstants.setForeground(tailStyle, tailFg)
  
  val nodeStyle: Style = addStyle(null, null)
  StyleConstants.setBackground(nodeStyle, nodeBg)
  val restStyle: Style = addStyle(null, null)
  StyleConstants.setBackground(restStyle, plainBg)
  
  val nucleusHighlightStyle: Style = addStyle(null, null)
  StyleConstants.setBackground(nucleusHighlightStyle, nucleusHighlightBg)
  val satelliteHighlightStyle: Style = addStyle(null, null)
  StyleConstants.setBackground(satelliteHighlightStyle, satelliteHighlightBg)
  val undefinedHighlightStyle: Style = addStyle(null, null)
  StyleConstants.setBackground(undefinedHighlightStyle, undefinedHighlightBg)
  
  val paraStyle: Style = addStyle(PARA_STYLE, null)
  StyleConstants.setFirstLineIndent(paraStyle, 32)
  StyleConstants.setLineSpacing(paraStyle, 0.3.toFloat)
  
  var fileName: String = _
  var title: String = _
  
  var startOffsetOfRemoval: Int = -1
  var lengthOfRemoval: Int = -1
  
  val rawLexicalChainList = new mutable.MutableList[RawLexicalChain]
  
  var synched = true
  
  addDocumentListener(this)
  
  def load(fileName: String) {
    this.fileName = fileName
    remove(0, getLength)
    val article = Resource.getArticle(fileName)
    title = article.title
    insertString(0, article.text, plainStyle)
    setParagraphAttributes(0, getLength, paraStyle, true)
  }
  
  def getParagraphs: mutable.MutableList[String] = {
    val list = new mutable.MutableList[String]
    for(i <- 0 until getDefaultRootElement.getElementCount) {
      val paraEle = getDefaultRootElement.getElement(i)
      list += getText(paraEle.getStartOffset, paraEle.getEndOffset - paraEle.getStartOffset)
    }
    list
  }
  
  def tag() {
    rawLexicalChainList.clear
    val caretPosition = if(displaying) Resource.editor.getCaretPosition else -1
    
    val text = getText(0, getLength)
    remove(0, getLength)
    insertString(0, text, plainStyle)
    setCharacterAttributes(0, getLength, plainStyle, false)
    tagChainList(Resource.getChainList(Resource.CHAIN_LIST), headStyle, tailStyle)
    tagConnList(Resource.getWordList(Resource.CONN_LIST), connStyle)
    
    if(displaying)
      Resource.editor.setCaretPosition(caretPosition)
  }
  
  def tagConnList(list: mutable.MutableList[String], style: Style): Unit = {
    val sentenceInfos = splitIntoSentences(getText(0, getLength))
    for(sentenceInfo <- sentenceInfos) {
      tagConnListForSentence(sentenceInfo.sentence, sentenceInfo.offset, list, connStyle)
    }
  }
  
  def tagConnListForSentence(sentence: String, offset: Int, list: mutable.MutableList[String], style: Style) {
    var from = 0
    var index = -1
    for(word <- list) {
      from = 0
      index = sentence.indexOf(word, from)
      while(index != -1) {
        if(isInPlainElement(word, index + offset) &&
            (index == 0 || isPunc(sentence.charAt(index - 1), MINOR_PUNCS) || isPunc(sentence.charAt(index - 1), MINOR_PUNCS))) {
          setCharacterAttributes(index + offset, word.length, style, false)
        }
        from = index + word.length
        index = sentence.indexOf(word, from)
      }
    }
  }
  
  def tagConnListForSpan(startOffset: Int, endOffset: Int, list: mutable.MutableList[String], style: Style) {
    val sentenceInfos = splitIntoSentences(getText(startOffset, endOffset - startOffset))
    for(sentenceInfo <- sentenceInfos) {
      tagConnListForSentence(sentenceInfo.sentence, sentenceInfo.offset + startOffset, list, connStyle)
    }
  }
  
  def deleteSpan(startOffset: Int, length: Int) {
    startOffsetOfRemoval = startOffset
    lengthOfRemoval = length
    remove(startOffset, length)
    synched = false
  }

  def isInPlainElement(word: String, from: Int): Boolean = {
    val element = getCharacterElement(from)
    if(isPlainElement(element) && element.getEndOffset - from >= word.length) true else false
  }
  
  def isPlainElement(element: Element): Boolean = StyleConstants.getForeground(element.getAttributes) == plainFg
  
  def isPlainSpan(startOffset: Int, endOffset: Int): Boolean = {
    val element = getCharacterElement(startOffset)
    !(element.getStartOffset == startOffset && element.getEndOffset == endOffset && !isPlainElement(element))
  }
  
  def getColorAtPosition(pos: Int): Color = StyleConstants.getForeground(getCharacterElement(pos).getAttributes)
  
  def tagChainList(list: mutable.MutableList[Chain], headStyle: Style, tailStyle: Style) {
    val sentenceInfos = splitIntoSentences(getText(0, getLength))
    for(sentenceInfo <- sentenceInfos) {
      tagChainListForSentence(sentenceInfo.sentence, sentenceInfo.offset, list, headStyle, tailStyle)
    }
  }
  
  val candidates = new mutable.MutableList[mutable.MutableList[WordInfo]]
  
  def tagChainListForSentence(sentence: String, offset: Int, chains: mutable.MutableList[Chain], headStyle: Style, tailStyle: Style) {
    candidates.clear
    var infos = new mutable.MutableList[WordInfo]
    for(chain <- chains) {
      infos.clear
      var chainWordIndex = 0
      var from = 0
      var currChainWord = chain.words.get(chainWordIndex).get
      var senWordIndex = sentence.indexOf(currChainWord, from)
      var firstChainWordSenIndex = -1
      while(senWordIndex != -1) {
        if(chainWordIndex == 0)
          firstChainWordSenIndex = senWordIndex
        infos += WordInfo(senWordIndex, currChainWord.length)
        if(chainWordIndex == chain.words.size - 1) {
          candidates += infos
          infos = new mutable.MutableList[WordInfo]
          chainWordIndex = 0
          from = firstChainWordSenIndex + chain.words.head.length
        }
        else {
          chainWordIndex += 1
          from = senWordIndex + currChainWord.length
        }
        currChainWord = chain.words.get(chainWordIndex).get
        senWordIndex = sentence.indexOf(currChainWord, from)
      }
    }
    
    val sorted = candidates.sortWith((a, b) => {
      val size = Math.min(a.size, b.size)
      var result = 0
      var i = 0
      while(i < size && result == 0) {
        result = a.get(i).get.beginIndex - b.get(i).get.beginIndex
        if(result == 0)
          result = -(a.get(i).get.length - b.get(i).get.length)
        i += 1
      }
      if(result < 0)
        true
      else if(result > 0)
        false
      else {
        if(a.size > b.size)
          true
        else
          false
      }
    })
    
    var from = 0
    var i = 0
    var startOffset = 0
    while(i < sorted.size) {
      val curr = sorted.get(i).get
      setCharacterAttributes(curr.head.beginIndex + offset, curr.head.length, headStyle, false)
      for(i <- 1 until curr.size)
        setCharacterAttributes(curr.get(i).get.beginIndex + offset, curr.get(i).get.length, tailStyle, false)
      from = curr.last.beginIndex + curr.last.length
      while(from < sentence.length && !isPunc(sentence.charAt(from), MINOR_PUNCS))
        from += 1
      while(i < sorted.size && sorted.get(i).get.head.beginIndex < from)
        i += 1
      if(i < sorted.size)
        rawLexicalChainList += RawLexicalChain(offset + startOffset, offset + from)
      else {
        if(isPunc(sentence.charAt(sentence.length - 1), MAJOR_PUNCS))
          rawLexicalChainList += RawLexicalChain(offset + startOffset, offset + sentence.length - 1)
        else 
          rawLexicalChainList += RawLexicalChain(offset + startOffset, offset + sentence.length)
      }
      startOffset = from + 1
    }
  }
  
  case class SentenceInfo(sentence: String, offset: Int)
  
  def splitIntoSentences(text: String): mutable.MutableList[SentenceInfo] = {
    val list = new mutable.MutableList[SentenceInfo]
    val paras = text.split("\n")
    var offset = 0
    for(para <- paras) {
      var from = 0
      for(i <- 0 until para.length) {
        if(isPunc(para.charAt(i), MAJOR_PUNCS) || i == para.length - 1) {
          list += SentenceInfo(para.substring(from, i + 1), offset)
          offset += i + 1 - from
          from = i + 1
        }
      }
      offset += 1
    }
    list
  }
  
  private def isPunc(char: Char, puncs: Seq[Char]): Boolean = puncs.indexOf(char) != -1
  
  def getLexemeList(startOffset: Int, endOffset: Int): mutable.MutableList[Lexeme] = {
    val list = new mutable.MutableList[Lexeme]
    var from = startOffset
    while(from < endOffset) {
      val element = getParagraphElement(from)
      collectLexemesFromElement(element, list, from, endOffset)
      from = element.getEndOffset
    }
    list
  }
  
  def getFilteredLexemeList(startOffset: Int, endOffset: Int): mutable.MutableList[Lexeme] = {
    val list = new ArrayList[Lexeme]
    var from = startOffset
    while(from < endOffset) {
      val element = getParagraphElement(from)
      collectLexemesFromElement(element, list, from, endOffset)
      from = element.getEndOffset
    }
    
    val toBeRemovedIndices = new mutable.MutableList[Int]
    if(list.size == 1 && list.head.category == "C") {
      toBeRemovedIndices += 0
    }
    else {
      for(i <- 1 until list.size) {
        if(list.apply(i).category == "C") {
          toBeRemovedIndices += i
        }
      }
    }
    var count = 0
    for(i <- toBeRemovedIndices.indices) {
      list.removeAtIndex(toBeRemovedIndices.apply(i) - count)
      count += 1
    }
    list
  }
  
  def collectLexemesFromElement(element: Element, list: mutable.MutableList[Lexeme], startOffset: Int, endOffset: Int): Unit = {
    var done = false
    var i = 0
    while(i < element.getElementCount && !done) {
      val child = element.getElement(i)
      if(child.getStartOffset >= startOffset && child.getEndOffset <= endOffset) {
        val category = determineElementCategory(child)
        if(category != null)
          list += new Lexeme(getText(child.getStartOffset, child.getEndOffset - child.getStartOffset), category, child.getStartOffset, child.getEndOffset - child.getStartOffset)
      }
      if(child.getEndOffset > endOffset)
        done = true
      i += 1
    }
  }
  
  def determineElementCategory(element: Element): String = {
    val word = getText(element.getStartOffset, element.getEndOffset - element.getStartOffset)
    StyleConstants.getForeground(element.getAttributes) match {
      case this.headFg => if(Resource.containedInChainList(Resource.CHAIN_LIST, word)) "H" else null
      case this.tailFg => if(Resource.containedInChainList(Resource.CHAIN_LIST, word)) "T" else null
      case this.connFg => if(Resource.getWordList(Resource.CONN_LIST).contains(word)) "C" else null
      case _ => null
    }
  }
  
  def getStandardSpan(startOffset: Int, endOffset: Int): (Int, Int) = {
    var i = 0
    var found = false
    while(i < rawLexicalChainList.size && !found) {
      if(doesIntersect(startOffset, endOffset, rawLexicalChainList.apply(i).startOffset, rawLexicalChainList.apply(i).endOffset)) {
        found = true
      }
      else {
        i += 1
      }
    }
    if(found)
      (rawLexicalChainList.apply(i).startOffset, rawLexicalChainList.apply(i).endOffset)
    else
      null
  }
  
  private def doesIntersect(startOffset1: Int, endOffset1: Int, startOffset2: Int, endOffset2: Int) =
    (startOffset2 >= startOffset1 && startOffset2 < endOffset1) || (startOffset1 >= startOffset2 && startOffset1 < endOffset2)
  
  def markHeadWord(startOffset: Int, length: Int) {
    setCharacterAttributes(startOffset, length, headStyle, false)
  }
  
  def markTailWord(startOffset: Int, length: Int) {
    setCharacterAttributes(startOffset, length, tailStyle, false)
  }
  
  def markSpan(startOffset: Int, endOffset: Int) {
    setCharacterAttributes(startOffset, endOffset - startOffset, nodeStyle, false)
    val list = getLexemeList(startOffset, endOffset)
    if(list.size == 1 && list.head.category == "C") {
      demarkLexeme(list.head)
    }
    else {
      for(i <- 1 until list.size) {
        if(list.apply(i).category == "C") {
          demarkLexeme(list.apply(i))
        }
      }
    }
  }
  
  def demarkSpan(startOffset: Int, endOffset: Int) {
    setCharacterAttributes(startOffset, endOffset - startOffset, restStyle, false)
    tagConnListForSpan(startOffset, endOffset, Resource.getWordList(Resource.CONN_LIST), connStyle)
  }
  
  def demarkLexeme(lex: Lexeme) {
    setCharacterAttributes(lex.startOffset, lex.length, plainStyle, false)
  }
  
  def highlightSpanAsNucleus(startOffset: Int, endOffset: Int) {
    setCharacterAttributes(startOffset, endOffset - startOffset, nucleusHighlightStyle, false)
  }
  
  def highlightSpanAsSatellite(startOffset: Int, endOffset: Int) {
    setCharacterAttributes(startOffset, endOffset - startOffset, satelliteHighlightStyle, false)
  }
  
  def highlightSpanAsUndefined(startOffset: Int, endOffset: Int) {
    setCharacterAttributes(startOffset, endOffset - startOffset, undefinedHighlightStyle, false)
  }
  
  case class WordInfo(beginIndex: Int, length: Int)

  def changedUpdate(event: DocumentEvent): Unit = {
  }

  def insertUpdate(event: DocumentEvent): Unit = {}

  def removeUpdate(event: DocumentEvent): Unit = {
    if(startOffsetOfRemoval >= 0 && lengthOfRemoval > 0) {
      ActionEventDispatcher.fireActionEvent(new ActionEvent(TextSpan(fileName, startOffsetOfRemoval, lengthOfRemoval), 0, Command.TEXT_SPAN_DELETED))
      startOffsetOfRemoval = -1
      lengthOfRemoval = -1
    }
  }

}

class Lexeme(val word: String, val category: String, val startOffset: Int, val length: Int) {
  override def toString = s"$word|$category"
}

case class RawLexicalChain(startOffset: Int, endOffset: Int)

case class TextSpan(fileName: String, startOffset: Int, length: Int)
