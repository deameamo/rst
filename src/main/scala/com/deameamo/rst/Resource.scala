package com.deameamo.rst

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Font}
import java.io.{File, FileInputStream}
import java.util.Properties
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, Transformer, TransformerFactory}

import com.deameamo.event.ActionEventDispatcher
import com.deameamo.html.HtmlElement
import com.deameamo.util.{ArrayList, FileUtil}
import org.jsoup.Jsoup
import org.w3c.dom.{Document, Element}

import scala.collection.mutable

object Resource extends ActionListener {
  
  def main(args: Array[String]) {
  }
  
  val DEFAULT_PARAMS_PATH = "conf/default.properties"
  var TAG_BASE: String = _
  var ARTICLE_BASE: String = _
  val NE_MAP_BASE: String = "nemap/"
  val TREE_BASE: String = "tree/"

  val neMapCache = new mutable.HashMap[String, mutable.HashMap[Int, NamedEntity]]
  val treeModelCache = new mutable.HashMap[String, RSTTreeModel]
  val colorMap = new mutable.HashMap[String, Color]
  val docCache = new mutable.HashMap[String, RSTDocument]
  
  val builder: DocumentBuilder = DocumentBuilderFactory.newInstance.newDocumentBuilder
  val transformer: Transformer = TransformerFactory.newInstance.newTransformer
  
  var tree: RSTTree = _
  def setTree(tree: RSTTree) { this.tree = tree }
  def getTree: RSTTree = tree
  
  var editor: RSTTextPane = _
  val patternTreeModel: LexicalChainPatternTreeModel = new LexicalChainPatternTreeModel
  
  var autoSort = true
  var forcedSave = false
  var autoGenerate = true
  var fontSize = 16
  var fontFamily = "Times New Roman"
  var defaultFont: Font = _
  var defaultItalicFont: Font = _

  private val KEY_TAG_BASE = "tag_base"
  private val KEY_ARTICLE_BASE = "article_base"
  private val KEY_AUTO_SORT = "autoSort"
  private val KEY_FORCED_SAVE = "forcedSave"
  private val KEY_AUTO_GENERATE = "autoGenerate"
  private val KEY_FONT_SIZE = "fontSize"
  private val KEY_FONT_FAMILY = "fontFamily"

  ActionEventDispatcher.addActionListener(this, Command.SAVE)
  
// load & save =============================================================================================
  def load(paramsPath: String) {
    treeModelCache.clear
    colorMap.clear
    docCache.clear
    loadParams(paramsPath)
    createNecessaryFiles()
    todoList = FileUtil.readFile(getTodoFilePath)
    doneList = FileUtil.readFile(getDoneFilePath)
    loadChainList(CHAIN_LIST)
    loadWordList(CONN_LIST)
    ActionEventDispatcher.fireActionEvent(Command.RESOURCE_LOADED)
  }
  
  def loadParams(paramsPath: String) {
    val props = new Properties()
    props.load(new FileInputStream(new File(paramsPath)))
    val names = props.propertyNames()
    while(names.hasMoreElements) {
      val name = names.nextElement().asInstanceOf[String]
      name match {
        case KEY_TAG_BASE => TAG_BASE = props.getProperty(name)
        case KEY_ARTICLE_BASE => ARTICLE_BASE = props.getProperty(name)
        case KEY_AUTO_SORT => autoSort = props.getProperty(name).toBoolean
        case KEY_FORCED_SAVE => forcedSave = props.getProperty(name).toBoolean
        case KEY_AUTO_GENERATE => autoGenerate = props.getProperty(name).toBoolean
        case KEY_FONT_SIZE => fontSize = props.getProperty(name).toInt
        case KEY_FONT_FAMILY => fontFamily = props.getProperty(name)
        case _ =>
          val ints = toIntValue(props.getProperty(name).split(","))
          colorMap.put(name, new Color(ints(0), ints(1), ints(2)))
      }
    }
    defaultFont = new Font(fontFamily, Font.PLAIN, fontSize)
    defaultItalicFont = new Font(fontFamily, Font.ITALIC, fontSize)
  }
  
  def createNecessaryFiles() {
    createDir(TAG_BASE)
//    createDir(TAG_BASE + "nemap")
    createDir(TAG_BASE + "tree")
    createFile(TAG_BASE + "chain")
    createFile(TAG_BASE + "conn")
//    createFile(TAG_BASE + "key")
//    createFile(TAG_BASE + "help")
    createFile(TAG_BASE + "done")
    createFile(TAG_BASE + "todo")
  }
  
  def createDir(dirPath: String) { if(!FileUtil.exists(dirPath)) FileUtil.makeDir(dirPath) }
  
  def createFile(filePath: String) { if(!FileUtil.exists(filePath)) FileUtil.makeFile(filePath) }
  
  def toIntValue(strings: Array[String]): Array[Int] = {
    val ints = new Array[Int](3)
    for((string, index) <- strings.zipWithIndex) {
      ints.update(index, string.toInt)
    }
    ints
  }
  
  def loadWordList(listName: String) {
    val list = getWordList(listName)
    val set = new mutable.HashSet[String]
    list.clear
    val in = FileUtil.getReader(TAG_BASE + listName)
    var line = in.readLine
    val temp = new mutable.MutableList[String]
    while(line != null) {
      if(line.length > 0 && !set.contains(line)) {
        set += line
        temp += line
      }
      line = in.readLine
    }
    list ++= temp.sortWith((a, b) => if(a.length > b.length) true else false)
  }
  
  def loadChainList(listName: String) {
    val list = getChainList(listName)
    val set = new mutable.HashSet[String]
    list.clear
    val in = FileUtil.getReader(TAG_BASE + listName)
    var line = in.readLine
    while(line != null) {
      if(line.length > 0 && !set.contains(line)) {
        set += line
        list += Chain(line)
      }
      line = in.readLine
    }
    sortChainList(listName)
  }
  
  def saveWordList(listName: String) {
    val list = getWordList(listName)
    val out = FileUtil.getWriter(TAG_BASE + listName)
    for(s <- list) {
      out.println(s)
    }
    FileUtil.closeWriter(out)
  }
  
  def saveChainList(listName: String) {
    val list = getChainList(listName)
    val out = FileUtil.getWriter(TAG_BASE + listName)
    for(c <- list) {
      out.println(c.toString)
    }
    FileUtil.closeWriter(out)
  }
  
// Document =============================================================================================
  
  def getRSTDocument(fileName: String): RSTDocument = {
    if(docCache.contains(fileName)) docCache(fileName)
    else {
      val doc = new RSTDocument
      doc.load(fileName)
      doc.tag()
      docCache.put(fileName, doc)
      doc
    }
  }
  
  def saveRSTDocument(doc: RSTDocument) {
    val bodyEle = new HtmlElement("body")
    
    val metaEle = new HtmlElement("div")
    metaEle.addAttr("id", "meta")
    val titleEle = new HtmlElement("h1")
    titleEle.text = doc.title
    metaEle.addChild(titleEle)
    
    val mainEle = new HtmlElement("div")
    mainEle.addAttr("id", "main")
    val list = doc.getParagraphs
    list.foreach(para => {
      val paraEle = new HtmlElement("p")
      paraEle.text = para
      mainEle.addChild(paraEle)
    })
    
    bodyEle.addChild(metaEle)
    bodyEle.addChild(mainEle)
    
    FileUtil.saveAsFile(bodyEle.toString, ARTICLE_BASE + doc.fileName, "utf-8")
  }
  
// Named Entity =============================================================================================
  def getNeMap(fileName: String): mutable.HashMap[Int, NamedEntity] = {
    if(neMapCache.contains(fileName)) {
      neMapCache(fileName)
    }
    else {
      val map = loadNeMap(fileName)
      neMapCache.put(fileName, map)
      map
    }
  }
  
  def loadNeMap(fileName: String): mutable.HashMap[Int, NamedEntity] = {
    val map = new mutable.HashMap[Int, NamedEntity]
    if(FileUtil.exists(TAG_BASE + NE_MAP_BASE + fileName)) {
      val in = FileUtil.getReader(TAG_BASE + NE_MAP_BASE + fileName)
      var line = in.readLine
      val pattern = """(\d+) (\d+) (\d)""".r
      while(line != null) {
        line match {
          case pattern(beginOffset, endOffset, category) => map.put(beginOffset.toInt, NamedEntity(beginOffset.toInt, endOffset.toInt, category.toInt))
          case _ => throw new Exception
        }
        line = in.readLine
      }
    }
    map
  }
  
// RST Tree =============================================================================================
  def getTreeModel(fileName: String): RSTTreeModel = {
    if(treeModelCache.contains(fileName)) {
      treeModelCache(fileName)
    }
    else {
      val model = new RSTTreeModel(fileName, RSTRootNode(fileName))
      treeModelCache.put(fileName, model)
      model
    }
  }
  
  def searchRelationNode(category: String): mutable.MutableList[RelationNode] = {
    val list = new mutable.MutableList[RelationNode]
    for(fileName <- doneList) {
      val model = getTreeModel(fileName)
      list ++= model.searchRelationNode(category)
    }
    list
  }
  
  def loadRSTTreeDocument(fileName: String): Document = {
    if(FileUtil.exists(TAG_BASE + TREE_BASE + fileName))
      builder.parse(new File(TAG_BASE + TREE_BASE + fileName))
    else
      null
  }
  
  def saveRSTTreeModel(model: RSTTreeModel) {
    val doc = builder.newDocument
    val xmlRoot = doc.createElement("root")
    val rstRoot = model.getRoot.asInstanceOf[RSTNode]
    doc.appendChild(xmlRoot)
    for(i <- 0 until rstRoot.getChildCount) {
      val child = rstRoot.getChildAt(i)
      child match {
        case node: RelationNode => buildXmlRelation(node, xmlRoot, doc)
        case node: LexicalChainInstanceNode => buildXmlChainInstance(node, xmlRoot, doc)
        case _ =>
      }
    }
    transformer.setOutputProperty(OutputKeys.INDENT, "no")
    transformer.transform(new DOMSource(doc), new StreamResult(FileUtil.getWriter(TAG_BASE + TREE_BASE + model.getFileName)))
  }
  
  def buildXmlRelation(rstRelation: RelationNode, xmlParent: Element, doc: Document) {
    val xmlRelation = doc.createElement("relation")
    xmlParent.appendChild(xmlRelation)
    xmlRelation.setAttribute("category", rstRelation.category)
    xmlRelation.setAttribute("role", rstRelation.role.toString)
    for(i <- 0 until rstRelation.getChildCount) {
      val child = rstRelation.getChildAt(i)
      child match {
        case node: RelationNode => buildXmlRelation(node, xmlRelation, doc)
        case node: LexicalChainInstanceNode => buildXmlChainInstance(node, xmlRelation, doc)
        case _ =>
      }
    }
  }
  
  def buildXmlChainInstance(rstChain: LexicalChainInstanceNode, xmlParent: Element, doc: Document) {
    val xmlChain = doc.createElement("chain")
    xmlParent.appendChild(xmlChain)
    xmlChain.setAttribute("role", rstChain.role.toString)
    xmlChain.setAttribute("startOffset", rstChain.startOffset.toString)
    xmlChain.setAttribute("endOffset", rstChain.endOffset.toString)
  }
  
  def getTreeFileNameList: mutable.MutableList[String] = {
    val list = new mutable.MutableList[String]
    val dir = new File(TAG_BASE + TREE_BASE)
    for(file <- dir.listFiles) {
      list += file.getName
    }
    list
  }
  
  def standardize() {
    doneList.foreach(getTreeModel(_).standardize())
  }
  
// pattern =============================================================================================
  
  def loadPatternTreeDocument: Document = {
    if(FileUtil.exists(TAG_BASE + "pattern.xml"))
      builder.parse(new File(TAG_BASE + "pattern.xml"))
    else
      null
  }
  
  def savePatternTreeModel(model: LexicalChainPatternTreeModel) {
    val doc = builder.newDocument
    val xmlRoot = doc.createElement("pattern")
    val modelRoot = model.getRoot.asInstanceOf[LexicalChainPatternNode]
    doc.appendChild(xmlRoot)
    for(i <- 0 until modelRoot.getChildCount) {
      val child = modelRoot.getChildAt(i)
      child match {
        case node: LexicalChainPatternNode => buildXmlPattern(node, xmlRoot, doc)
        case node: LexicalChainNode => buildXmlChain(node, xmlRoot, doc)
        case _ =>
      }
    }
    transformer.setOutputProperty(OutputKeys.INDENT, "no")
    transformer.transform(new DOMSource(doc), new StreamResult(FileUtil.getWriter(TAG_BASE + "pattern.xml")))
  }
  
  def buildXmlPattern(modelPattern: LexicalChainPatternNode, xmlParent: Element, doc: Document) {
    val xmlPattern = doc.createElement("pattern")
    xmlParent.appendChild(xmlPattern)
    xmlPattern.setAttribute("name", modelPattern.patternName)
    for(i <- 0 until modelPattern.getChildCount) {
      val child = modelPattern.getChildAt(i)
      child match {
        case node: LexicalChainPatternNode => buildXmlPattern(node, xmlPattern, doc)
        case node: LexicalChainNode => buildXmlChain(node, xmlPattern, doc)
        case _ =>
      }
    }
  }
  
  def buildXmlChain(modelChain: LexicalChainNode, xmlParent: Element, doc: Document) {
    val xmlChain = doc.createElement("chain")
    xmlParent.appendChild(xmlChain)
    xmlChain.setAttribute("lexemes", modelChain.lexemes.mkString(","))
    for(i <- modelChain.positions.indices) {
      val modelPosition = modelChain.positions.get(i).get
      val xmlPosition = doc.createElement("position")
      xmlChain.appendChild(xmlPosition)
      xmlPosition.setAttribute("fileName", modelPosition.fileName)
      xmlPosition.setAttribute("startOffset", modelPosition.startOffset.toString)
      xmlPosition.setAttribute("endOffset", modelPosition.endOffset.toString)
    }
  }
  
// others =============================================================================================
  def getArticle(fileName: String): Article = {
    val doc = Jsoup.parse(new File(ARTICLE_BASE + fileName), "utf-8")
    val buf = new mutable.MutableList[String]
    val title = doc.getElementById("meta").getElementsByTag("h1").first.text
    val pElements = doc.getElementById("main").getElementsByTag("p")
    for(i <- 0 until pElements.size) {
      val pEle = pElements.get(i)
      val trimmed = trim(pEle.text)
      if(trimmed.length > 0)
        buf += trimmed
    }
    Article(title, buf.mkString("\n"))
  }
  
  def getColor(name: String): Color = colorMap(name)
  
// helpers =============================================================================================
  val BLANK_CHAR = '　'
  
  def trim(input: String): String = {
    val trimmed = input.trim
    var start = 0
    var found = false
    while(start < trimmed.length && !found) {
      if(trimmed.charAt(start) == BLANK_CHAR)
        start += 1
      else
        found = true
    }
    var end = trimmed.length - 1
    found = false
    while(end > -1 && !found) {
      if(trimmed.charAt(end) == BLANK_CHAR)
        end -= 1
      else
        found = true
    }
    if(end == -1)
      ""
    else
      trimmed.substring(start, end + 1)
  }
  
  def areLexemesEqual(a: mutable.MutableList[Lexeme], b: mutable.MutableList[Lexeme]): Boolean = {
    if(a.size == b.size) {
      var equal = true
      var i = 0
      while(i < a.size && equal) {
        val aLex = a.get(i).get
        val bLex = b.get(i).get
        if(aLex.word != bLex.word || aLex.category != bLex.category)
          equal = false
        i += 1
      }
      equal
    }
    else
      false
  }

// word & chain list =============================================================================================
  val KEY_LIST = "key"
  val AUX_LIST = "help"
  val CHAIN_LIST = "chain"
  val CONN_LIST = "conn"
  
  var keyList = new ArrayList[String]
  var auxList = new ArrayList[String]
  var chainList = new ArrayList[Chain]
  var connList = new ArrayList[String]
  
  def contains(listName: String, word: String): Boolean = {
    listName match {
      case KEY_LIST => keyList.indexOf(word) != -1 || connList.indexOf(word) != -1 || auxList.indexOf(word) != -1
      case AUX_LIST => keyList.indexOf(word) != -1 || connList.indexOf(word) != -1 || auxList.indexOf(word) != -1
      case CHAIN_LIST => keyList.indexOf(word) != -1
      case CONN_LIST => keyList.indexOf(word) != -1 || connList.indexOf(word) != -1 || auxList.indexOf(word) != -1
      case _ => false
    }
  }
  
  def getWordList(listName: String): ArrayList[String] = {
    listName match {
      case KEY_LIST => keyList
      case AUX_LIST => auxList
      case CONN_LIST => connList
      case _ => null
    }
  }
  
  def getChainList(listName: String): ArrayList[Chain] = {
    listName match {
      case CHAIN_LIST => chainList
      case _ => null
    }
  }
  
  def addWord(listName: String, word: String): Int = {
    val list = getWordList(listName)
    if(list.indexOf(word) == -1) {
      var found = false
      var index = 0
      while(!found && index < list.size) {
        if(list.get(index).get.length < word.length)
          found = true
        else
          index += 1
      }
      list += word
      var i = list.length - 1
      while(i > index) {
        list.update(i, list.get(i - 1).get)
        i -= 1
      }
      list.update(index, word)
      index
    }
    else
      -1
  }
  
  def removeWord(listName: String, index: Int) {
    getWordList(listName).removeAtIndex(index)
  }
  
  def addChain(listName: String, string: String): Boolean = {
    val list = getChainList(listName)
    if(indexOfChain(list, string) == -1) {
      list += Chain(string)
      sortChainList(listName)
      true
    }
    else
      false
  }
  
  def removeChain(listName: String, index: Int) {
    getChainList(listName).removeAtIndex(index)
  }
  
  private def sortChainList(listName: String) {
    if(!autoSort)
      return
    val list = getChainList(listName)
    val temp = list.sortWith((a, b) => {
      val size = Math.min(a.words.size, b.words.size)
      var result = 0
      var i = 0
      while(i < size && result == 0) {
        result = a.words.get(i).get.compareTo(b.words.get(i).get)
        i += 1
      }
      if(result < 0)
        true
      else if(result > 0)
        false
      else {
        if(a.words.size < b.words.size)
          true
        else
          false
      }
    })
    list.clear
    list ++= temp
  }
  
  def moveUpWord(listName: String, i: Int): Boolean = {
    val list = getWordList(listName)
    if(i != 0) {
      val temp = list.get(i).get
      if(temp.length == list.get(i - 1).get.length) {
        list.update(i, list.get(i - 1).get)
        list.update(i - 1, temp)
        true
      }
      else
        false
    }
    else
      false
  }
  
  def moveDownWord(listName: String, i: Int): Boolean = {
    val list = getWordList(listName)
    if(i != list.length - 1) {
      val temp = list.get(i).get
      if(temp.length == list.get(i + 1).get.length) {
        list.update(i, list.get(i + 1).get)
        list.update(i + 1, temp)
        true
      }
      else
        false
    }
    else
      false
  }
  
  def moveUpChain(listName: String, i: Int): Boolean = {
    val list = getChainList(listName)
    if(i != 0) {
      val temp = list.get(i).get
      list.update(i, list.get(i - 1).get)
      list.update(i - 1, temp)
      true
    }
    else
      false
  }
  
  def moveDownChain(listName: String, i: Int): Boolean = {
    val list = getChainList(listName)
    if(i != list.length - 1) {
      val temp = list.get(i).get
      list.update(i, list.get(i + 1).get)
      list.update(i + 1, temp)
      true
    }
    else
      false
  }
  
  def replaceWordList(listName: String, text: String) {
    val list = getWordList(listName)
    list.clear
    val words = text.split("\n")
    val temp = new mutable.MutableList[String]
    for(word <- words) {
      if(word.length > 0)
        temp += word
    }
    list ++= temp.sortWith((a, b) => if(a.length > b.length) true else false)
  }
  
  def replaceChainList(listName: String, text: String) {
    val list = getChainList(listName)
    list.clear
    val strings = text.split("\n")
    for(string <- strings) {
      if(string.length > 0)
        list += Chain(string)
    }
  }
  
  def indexOfChain(list: mutable.MutableList[Chain], targetString: String): Int = {
    var i = 0
    var found = false
    while(i < list.length && !found) {
      if(list.get(i).get.string == targetString)
        found = true
      else
        i += 1
    }
    if(found)
      i
    else
      -1
  }
  
  def containedInChainList(listName: String, word: String): Boolean = {
    val list = getChainList(listName)
    var contained = false
    var i = 0
    while(i < list.length && !contained) {
      contained = list.get(i).get.words.contains(word)
      i += 1
    }
    contained
  }
  
  def filterByDoneList(original: mutable.MutableList[LexicalChainPosition]): mutable.MutableList[LexicalChainPosition] = {
    val filtered = new mutable.MutableList[LexicalChainPosition]
    original.foreach(pos => {
      if(doneList.contains(pos.fileName))
        filtered += pos
    })
    filtered
  }
  
// todo- & done- list =============================================================================================
  val TODO_LIST = "TODO_LIST"
  val DONE_LIST = "DONE_LIST"
  
  var todoList = new mutable.MutableList[String]
  var doneList = new mutable.MutableList[String]
  
  def getTodoFilePath: String = TAG_BASE + "todo"
  def getDoneFilePath: String = TAG_BASE + "done"
  
  def getFileList(listName: String): mutable.MutableList[String] = {
    listName match {
      case TODO_LIST => todoList
      case DONE_LIST => doneList
      case _ => null
    }
  }
  
  def addFile(listName: String, fileName: String): mutable.MutableList[String] = {
    listName match {
      case TODO_LIST =>
        todoList += fileName
        todoList = todoList.sortWith((a, b) => if(a.compareTo(b) < 0) true else false)
        saveFileList(todoList, getTodoFilePath)
        todoList
      case DONE_LIST =>
        doneList += fileName
        doneList = doneList.sortWith((a, b) => if(a.compareTo(b) < 0) true else false)
        saveFileList(doneList, getDoneFilePath)
        doneList
      case _ => null
    }
  }
  
  def removeFile(listName: String, i: Int): mutable.MutableList[String] = {
    listName match {
      case TODO_LIST =>
        todoList = todoList.take(i) ++= todoList.drop(i + 1)
        saveFileList(todoList, getTodoFilePath)
        todoList
      case DONE_LIST =>
        doneList = doneList.take(i) ++= doneList.drop(i + 1)
        saveFileList(doneList, getDoneFilePath)
        doneList
      case _ => null
    }
  }
  
  def saveFileList(list: mutable.MutableList[String], filePath: String) {
    val out = FileUtil.getWriter(filePath)
    for(name <- list) {
      out.println(name)
    }
    FileUtil.closeWriter(out)
  }
  
// rules & conjunctions ======================================================================================
  val RULE = "rule"
  val CONJUNCTIONS = "conjunctions"
  
  def exportRules() {
    val out = FileUtil.getWriter(TAG_BASE + RULE)
    for(fileName <- doneList) {
      getTreeModel(fileName).getRoot.asInstanceOf[RSTNode].exportRule(out)
    }
    FileUtil.closeWriter(out)
  }
  
  def exportConjunctions() {
    val map = new mutable.HashMap[String, RelationInfo]
    for(fileName <- doneList) {
      getTreeModel(fileName).getRoot.asInstanceOf[RSTNode].exportConjunctions(map)
    }
    val out = FileUtil.getWriter(TAG_BASE + CONJUNCTIONS)
    map.values.foreach(info => {
      out.println(s"${info.conjunction}[${info.count}]: ${info.getSortedCounters.mkString(", ")}")
    })
    FileUtil.closeWriter(out)
  }

// handle action =============================================================================================
  ActionEventDispatcher.addActionListener(this, Command.SYNCH)
  
  def actionPerformed(event: ActionEvent) {
    event.getActionCommand match {
      case Command.SYNCH =>
        for(key <- treeModelCache.keys) {
          getRSTDocument(key).tag()
        }
        ActionEventDispatcher.fireActionEvent(Command.SYNCHED)
      case Command.SAVE =>
        for(model <- treeModelCache.values) {
          if(forcedSave || !model.synched)
            saveRSTTreeModel(model)
        }
        for(doc <- docCache.values) {
          if(!doc.synched)
            saveRSTDocument(doc)
        }
      case _ =>
    }
  }
}

case class Article(title: String, text: String)

case class NamedEntity(beginOffset: Int, endOffset: Int, category: Int)

case class Chain(string: String) {
  val words = new mutable.MutableList[String]
  words ++= string.split("\\-")
  override def toString: String = string
}
