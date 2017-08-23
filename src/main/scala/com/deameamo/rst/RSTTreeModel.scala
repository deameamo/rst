package com.deameamo.rst

import java.awt.event.ActionEvent
import java.io.PrintWriter
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeModel, TreePath}

import com.deameamo.event.ActionEventDispatcher
import org.w3c.dom.Element

import scala.collection.mutable

class RSTTreeModel(val fileName: String, root: RSTRootNode) extends DefaultTreeModel(root: DefaultMutableTreeNode) {
  
  root.setModel(this)
  build()
  root.rawNode.generate()
  root.updateName()
  
  var synched = true
  
  def getFileName: String = fileName
  
  def build() {
    val document = Resource.loadRSTTreeDocument(fileName)
    if(document != null) {
      val docEle = document.getDocumentElement
      val childNodes = docEle.getChildNodes
      for(i <- 0 until childNodes.getLength) {
        val child = childNodes.item(i).asInstanceOf[Element]
        child.getNodeName match {
          case "relation" => root.addChild(buildRelationNode(child))
          case "chain" => root.addChild(buildChainNode(child))
        }
      }
    }
  }
  
  def standardize() {
    root.standardize()
  }
  
  def isRaw: Boolean = root.isRaw
  
  def generate() { root.rawNode.generate() }
  
  def getTreePath(startOffset: Int): TreePath = {
    val targetNode = root.findChainNode(startOffset)
    if(targetNode != null)
      new TreePath(getPathToRoot(targetNode).asInstanceOf[Array[Object]])
    else
      null
  }
  
  def getAllChainInstanceNodes: mutable.MutableList[LexicalChainInstanceNode] = {
    val list = new mutable.MutableList[LexicalChainInstanceNode]
    root.extractAllChainInstanceNodes(list)
    list
  }

  def buildRelationNode(ele: Element): RelationNode = {
    val node = new RelationNode(ele.getAttribute("category"), ele.getAttribute("role").toInt, this)
    val childNodes = ele.getChildNodes
    for(i <- 0 until childNodes.getLength) {
      val child = childNodes.item(i).asInstanceOf[Element]
      child.getNodeName match {
        case "relation" => node.addChild(buildRelationNode(child))
        case "chain" => node.addChild(buildChainNode(child))
      }
    }
//    node.updateName
    node
  }

  def adjustTree(span: TextSpan) {
    root.adjust(span)
  }

  def buildChainNode(ele: Element): LexicalChainInstanceNode = {
    val startOffset = {
//      if(fileName == "460.html")
//        ele.getAttribute("startOffset").toInt + 27
//      else
        ele.getAttribute("startOffset").toInt
    }
    val endOffset = {
//      if(fileName == "460.html")
//        ele.getAttribute("endOffset").toInt + 27
//      else
        ele.getAttribute("endOffset").toInt
    }
    new LexicalChainInstanceNode(ele.getAttribute("role").toInt, startOffset, endOffset, this, fileName)
  }


  def isSpanMarked(spanStartOffset: Int, spanEndOffset: Int): Boolean = getRoot.asInstanceOf[RSTRootNode].isSpanMarked(spanStartOffset, spanEndOffset)

  def refresh(): Unit = root.refresh()

  def isComplete: Boolean = root.isComplete

  def isLegal: Boolean = root.isLegal

  def isEmpty: Boolean = root.isEmpty

  def searchRelationNode(category: String): mutable.MutableList[RelationNode] = {
    val list = new mutable.MutableList[RelationNode]
    root.searchRelationNode(category, list)
    list
  }
}

abstract class RSTNode extends DefaultMutableTreeNode {

  val UNDEFINED_ROLE: Int = -1
  val NUCLEUS_ROLE = 1
  val SATELLITE_ROLE = 0

  var role: Int = UNDEFINED_ROLE
  var startOffset: Int = Int.MaxValue
  var model: RSTTreeModel = _

  def displayRole(role: Int): String = {
    role match {
      case UNDEFINED_ROLE => "未定义"
      case NUCLEUS_ROLE => "核心"
      case SATELLITE_ROLE => "卫星"
    }
  }

  def switchRole() {
    if(role == NUCLEUS_ROLE)
      role = SATELLITE_ROLE
    else if(role == SATELLITE_ROLE)
      role = NUCLEUS_ROLE
    updateName()
  }

  def canAddChild(incoming: RSTNode): Boolean
  def addChild(incoming: RSTNode) {}
  def removeChild(leaving: RSTNode) {}
  def delete()
  def updateName()
  def highlight()
  def dehighlight()
  def highlightAsNucleus()
  def highlightAsSatellite()
  def refresh()
  def isSpanMarked(spanStartOffset: Int, spanEndOffset: Int): Boolean
  def generateRule: String
  def generateRuleHead: String
  def isComplete: Boolean
  def isLegal: Boolean
  def adjust(span: TextSpan)

  def reCalculateParentStartOffset() {
    val parent = getParent.asInstanceOf[RSTNode]
    if(parent != null && !parent.isInstanceOf[RSTRootNode]) {
      if(parent.getChildCount == 1)
        parent.startOffset = parent.getChildAt(0).asInstanceOf[RSTNode].startOffset
      else if(parent.getChildCount == 2) {
        val startOffset0 = parent.getChildAt(0).asInstanceOf[RSTNode].startOffset
        val startOffset1 = parent.getChildAt(1).asInstanceOf[RSTNode].startOffset
        if(startOffset0 != Int.MaxValue && startOffset1 != Int.MaxValue)
          parent.startOffset = Math.min(startOffset0, startOffset1)
        else
          parent.startOffset = Math.max(startOffset0, startOffset1)
      }
      parent.reCalculateParentStartOffset()
    }
  }

  def switchPosition(another: RSTNode) {
    val thisParent = getParent.asInstanceOf[RSTNode]
    val thisIndex = thisParent.getIndex(this)
    val anotherParent = another.getParent.asInstanceOf[RSTNode]
    val anotherIndex = anotherParent.getIndex(another)

    if(thisParent == anotherParent) {
      if(thisIndex < anotherIndex) {
        thisParent.removeChild(another)
        model.insertNodeInto(another, thisParent, thisIndex)
        thisParent.removeChild(this)
        model.insertNodeInto(this, thisParent, anotherIndex)
      }
      else {
        thisParent.removeChild(this)
        model.insertNodeInto(this, thisParent, anotherIndex)
        thisParent.removeChild(another)
        model.insertNodeInto(another, thisParent, thisIndex)
      }
      thisParent.updateName()
    }
    else {
      thisParent.removeChild(this)
      anotherParent.removeChild(another)
      model.insertNodeInto(this, anotherParent, anotherIndex)
      model.insertNodeInto(another, thisParent, thisIndex)
      thisParent.updateName()
      anotherParent.updateName()
      val thisRole = this.role
      val anotherRole = another.role
      this.role = anotherRole
      another.role = thisRole
    }
    this.updateName()
    another.updateName()
  }

  def generateIOString: String = {
    if(getParent.isInstanceOf[RSTRootNode])
      s"/"
    else {
      val parent = getParent.asInstanceOf[RelationNode]
      val index = 2 - parent.getIndex(this)
      if(parent.isNSRelation)
        if(role == NUCLEUS_ROLE) s"/$index" else s"$index/"
      else
        s"$index/$index"
    }
  }

  def exportRule(out: PrintWriter)

  def exportConjunctions(map: mutable.HashMap[String, RelationInfo])

  def getInfo: Info

  val dummyInfo = Info(0, 0, 0, 0)

  def standardize()
}

case class Info(var numberOfRelation: Int, var numberOfLeaf: Int, var height: Int, var numberOfEleRelation: Int) {
  def += (another: Info) {
    this.numberOfRelation += another.numberOfRelation
    this.numberOfLeaf += another.numberOfLeaf
    this.numberOfEleRelation += another.numberOfEleRelation
  }

  override def toString = s"[$numberOfRelation:$height:$numberOfEleRelation]"
}

class RelationInfo(val conjunction: String) {

  var count = 0
  val map = new mutable.HashMap[String, RelationCounter]

  def addRelation(category: String) {
    if(!map.contains(category)) {
      map.put(category, RelationCounter(category, 0))
    }
    map.apply(category).count += 1
    count += 1
  }

  def getSortedCounters: mutable.MutableList[RelationCounter] = {
    val list = new mutable.MutableList[RelationCounter]
    list ++= map.values
    val sorted = list.sortWith((a, b) => {
      if(a.count > b.count)
        true
      else if(a.count < b.count)
        false
      else
        if(a.category.compareTo(b.category) < 0) true else false
    })
    sorted
  }
}

case class RelationCounter(category: String, var count: Int) {

  override def toString = s"$category|$count"
}

case class RSTRootNode(fileName: String) extends RSTNode {

  var relationChildCount = 0

  var name = ""

  setUserObject(fileName)

  val rawNode = new RawNode(fileName)

  def setModel(model: RSTTreeModel) {
    this.model = model
    rawNode.setModel(model)
    model.insertNodeInto(rawNode, this, 0)
  }

  def canAddChild(incoming: RSTNode): Boolean = incoming.getParent != this

  override def removeChild(leaving: RSTNode) {
    model.removeNodeFromParent(leaving)
    if(leaving.isInstanceOf[RelationNode])
      relationChildCount -= 1
  }

  override def addChild(incoming: RSTNode) {
    if(canAddChild(incoming)) {
      incoming.role = UNDEFINED_ROLE
//      incoming.updateName
      if(incoming.isInstanceOf[RelationNode]) {
        model.insertNodeInto(incoming, this, relationChildCount)
        relationChildCount += 1
      }
      else {
        val chain = incoming.asInstanceOf[LexicalChainInstanceNode]
        model.insertNodeInto(chain, this, getChildCount - 1)
        if(chain.raw) {
          chain.raw = false
          ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
          ActionEventDispatcher.fireActionEvent(new ActionEvent(chain, 0, Command.LEXICAL_CHAIN_INSTANCE_ADDED))
        }
      }
    }
  }

  def highlight() { refresh() }

  def dehighlight() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].dehighlight() }

  def highlightAsNucleus() {}
  def highlightAsSatellite() {}

  def delete() {
    val count = getChildCount - 1
    for(_ <- 0 until count)
      getChildAt(0).asInstanceOf[RSTNode].delete()
    relationChildCount = 0
  }

  def isSpanMarked(spanStartOffset: Int, spanEndOffset: Int): Boolean = {
    var marked = false
    var i = 0
    while(!marked && i < getChildCount) {
      marked = getChildAt(i).asInstanceOf[RSTNode].isSpanMarked(spanStartOffset, spanEndOffset)
      i += 1
    }
    marked
  }

  def updateName() {
    for(i <- 0 until getChildCount)
      getChildAt(i).asInstanceOf[RSTNode].updateName()

    val newName = s"$fileName ${getInfo.toString}"
    if(name != newName) {
      name = newName
      setUserObject(name)
      model.nodeChanged(this)
    }
  }

  def refresh() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].refresh() }

  def generateRule = null
  def generateRuleHead = null

  override def generateIOString: String = null

  def exportRule(out: PrintWriter) {
    for(i <- 0 until getChildCount - 1)
      getChildAt(i).asInstanceOf[RSTNode].exportRule(out)
  }

  def exportConjunctions(map: mutable.HashMap[String, RelationInfo]) {
    for(i <- 0 until getChildCount - 1)
      getChildAt(i).asInstanceOf[RSTNode].exportConjunctions(map)
  }

  def findChainNode(startOffset: Int): LexicalChainInstanceNode = {
    var targetNode: LexicalChainInstanceNode = null
    var i = 0
    while(i < getChildCount - 1 && targetNode == null) {
      val child = getChildAt(i)
      child match {
        case chainChild: LexicalChainInstanceNode =>
          if (chainChild.startOffset == startOffset)
            targetNode = chainChild
        case _ =>
          val relationChild = child.asInstanceOf[RelationNode]
          targetNode = relationChild.findChainNode(startOffset)
      }
      i += 1
    }
    targetNode
  }

  def extractAllChainInstanceNodes(list: mutable.MutableList[LexicalChainInstanceNode]) {
    for(i <- 0 until getChildCount - 1) {
      val child = getChildAt(i)
      child match {
        case node: LexicalChainInstanceNode => list += node
        case _ => child.asInstanceOf[RelationNode].extractAllChainInstanceNodes(list)
      }
    }
  }

  def isComplete: Boolean = {
    if(getChildCount == 1)
      false
    else if(getChildCount == 2) {
      var complete = true
      var i = 0
      while(i < getChildCount - 1 && complete) {
        complete = getChildAt(i).asInstanceOf[RSTNode].isComplete
        i += 1
      }
      complete
    }
    else
      false
  }

  def isLegal: Boolean = {
    if(!rawNode.isLegal)
      return false
    var legal = true
    var i = 0
    while(i < getChildCount && legal) {
      legal = getChildAt(i).asInstanceOf[RSTNode].isLegal
      i += 1
    }
    legal
  }

  def isRaw: Boolean = rawNode.getChildCount == 0

  def isEmpty: Boolean = getChildCount == 1

  def adjust(span: TextSpan) { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].adjust(span) }

  def getInfo: Info = {
    val info = Info(0, 0, 0, 0)
    var maxHeight = -1
    for(i <- 0 until getChildCount) {
      val childInfo = getChildAt(i).asInstanceOf[RSTNode].getInfo
      info += childInfo
      maxHeight = Math.max(maxHeight, childInfo.height)
    }
    info.height += maxHeight
    info
  }

  def standardize() {
    for(i <- 0 until getChildCount) {
      getChildAt(i).asInstanceOf[RSTNode].standardize()
    }
  }

  def searchRelationNode(category: String, list: mutable.MutableList[RelationNode]){
    for(i <- 0 until getChildCount - 1) {
      val child = getChildAt(i)
      child match {
        case node: RelationNode =>
          if (node.category.toLowerCase.startsWith(category.toLowerCase)) {
            list += node
          }
          node.searchRelationNode(category, list)
        case _ =>
      }
    }
  }
}

class RelationNode(initCategory: String, initRole: Int, _model: RSTTreeModel) extends RSTNode {

  val CAPACITY = 2
  model = _model
  var name = ""

  var category: String = initCategory
  role = initRole
//  model = _model
  updateName()

  def isNSRelation: Boolean = (category != "Conjunction" && category != "Contrast" && category != "Disjunction" && category != "Joint"
      && category != "List" && category != "Multinuclear Restatement" && category != "Sequence")

  def isSN: Boolean = {
    if(isNSRelation) {
      if(getChildCount == CAPACITY)
        if(getChildAt(0).asInstanceOf[RSTNode].role == SATELLITE_ROLE) true else false
      else
        false
    }
    else
      false
  }

  def setCategory(newCategory: String) {
    val isNSRelationBefore = isNSRelation
    category = newCategory
    if(isNSRelationBefore && !isNSRelation) {
      for(i <- 0 until getChildCount) {
        val child = getChildAt(i).asInstanceOf[RSTNode]
        if(child.role == SATELLITE_ROLE)
          child.switchRole()
      }
    }
    else if(!isNSRelationBefore && isNSRelation && getChildCount == CAPACITY) {
      getChildAt(1).asInstanceOf[RSTNode].switchRole()
    }
    updateName()
  }

  def switchNS() {
    if(isNSRelation) {
      for(i <- 0 until getChildCount)
        getChildAt(i).asInstanceOf[RSTNode].switchRole()
    }
    updateName()
  }

  def updateName() {
    val newName = s"$category[$getChildCount]"
    if(name != newName) {
      name = newName
      setUserObject(newName)
    }
    model.nodeChanged(this)
  }

  def canAddChild(incoming: RSTNode): Boolean = incoming.getParent != this && getChildCount < CAPACITY && !model.getPathToRoot(this).contains(incoming)

  override def addChild(incoming: RSTNode) {
    if(canAddChild(incoming)) {
      getChildCount match {
        case 0 =>
          if(incoming.role == UNDEFINED_ROLE)
            incoming.role = NUCLEUS_ROLE
          incoming.updateName()
          startOffset = incoming.startOffset
          model.insertNodeInto(incoming, this, 0)
        case 1 =>
          val incumbent = getChildAt(0).asInstanceOf[RSTNode]
          if(isNSRelation) {
            if(incumbent.role == NUCLEUS_ROLE)
              incoming.role = SATELLITE_ROLE
            else
              incoming.role = NUCLEUS_ROLE
          }
          else
            incoming.role = NUCLEUS_ROLE

          incoming.updateName()

          if(incoming.startOffset == Int.MaxValue) {
            model.insertNodeInto(incoming, this, 1)
          }
          else {
            if(incumbent.startOffset < incoming.startOffset) {
              model.insertNodeInto(incoming, this, 1)
            }
            else {
              startOffset = incoming.startOffset
              model.insertNodeInto(incoming, this, 0)
            }
          }
      }
      reCalculateParentStartOffset()
      updateName()

      incoming match {
        case chain: LexicalChainInstanceNode =>
          if (chain.raw) {
            chain.raw = false
            ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
            ActionEventDispatcher.fireActionEvent(new ActionEvent(chain, 0, Command.LEXICAL_CHAIN_INSTANCE_ADDED))
          }
        case _ =>
      }
    }
  }

  override def removeChild(leaving: RSTNode) {
    if(getChildCount == CAPACITY) {
      val remaining = getChildAt(1 - getIndex(leaving)).asInstanceOf[RSTNode]
      startOffset = remaining.startOffset
    }
    else {
      startOffset = Int.MaxValue
    }
    reCalculateParentStartOffset()
    model.removeNodeFromParent(leaving)
    updateName()
  }

  def delete() {
    val count = getChildCount
    for(_ <- 0 until count)
      getChildAt(0).asInstanceOf[RSTNode].delete()

    startOffset = Int.MaxValue
    reCalculateParentStartOffset()
    val parent = getParent.asInstanceOf[RSTNode]
    parent.removeChild(this)
    parent.updateName()
  }

  def disband() {
    val root = model.getRoot.asInstanceOf[RSTRootNode]
    val count = getChildCount
    for(_ <- 0 until count) {
      val child = getChildAt(0).asInstanceOf[RSTNode]
      model.removeNodeFromParent(child)
      root.addChild(child)
    }
    startOffset = Int.MaxValue
    reCalculateParentStartOffset()
    val parent = getParent.asInstanceOf[RSTNode]
    parent.removeChild(this)
  }

  def highlight() {
    if(startOffset != Int.MaxValue)
      Resource.editor.setCaretPosition(startOffset)
    for(i <- 0 until getChildCount) {
      val child = getChildAt(i).asInstanceOf[RSTNode]
      if(child.role == NUCLEUS_ROLE)
        child.highlightAsNucleus()
      else if(child.role == SATELLITE_ROLE)
        child.highlightAsSatellite()
    }
  }

  def dehighlight() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].dehighlight() }

  def highlightAsNucleus() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].highlightAsNucleus() }

  def highlightAsSatellite() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].highlightAsSatellite() }

  def refresh() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].refresh() }

  def isSpanMarked(spanStartOffset: Int, spanEndOffset: Int): Boolean = {
    var marked = false
    var i = 0
    while(!marked && i < getChildCount) {
      marked = getChildAt(i).asInstanceOf[RSTNode].isSpanMarked(spanStartOffset, spanEndOffset)
      i += 1
    }
    marked
  }

  def generateRule: String = {
    if(getChildCount == CAPACITY)
      s"$generateRuleHead:${getChildAt(0).asInstanceOf[RSTNode].generateRuleHead},${getChildAt(1).asInstanceOf[RSTNode].generateRuleHead}"
    else
      null
  }

  def generateRuleHead: String = s"[$category:$generateIOString]"

  def exportRule(out: PrintWriter) {
    val rule = generateRule
    if(rule != null)
      out.println(rule)
    for(i <- 0 until getChildCount)
      getChildAt(i).asInstanceOf[RSTNode].exportRule(out)
  }

  def exportConjunctions(map: mutable.HashMap[String, RelationInfo]) {
    if(getChildCount != 2)
      return
    getChildAt(0) match {
      case child: LexicalChainInstanceNode =>
        val conj = getConjunction(child)
        if (conj != null && !getParent.isInstanceOf[RSTRootNode]) {
          addConjunction(conj, getParent.asInstanceOf[RelationNode].category, map)
        }
      case _ =>
        getChildAt(0).asInstanceOf[RelationNode].exportConjunctions(map)
    }

    getChildAt(1) match {
      case child: LexicalChainInstanceNode =>
        val conj = getConjunction(child)
        if (conj != null && !getParent.isInstanceOf[RSTRootNode]) {
          addConjunction(conj, category, map)
        }
      case _ =>
        getChildAt(1).asInstanceOf[RelationNode].exportConjunctions(map)
    }
  }

  def addConjunction(conj: String, category: String, map: mutable.HashMap[String, RelationInfo]) {
    if(!map.contains(conj)) {
      map.put(conj, new RelationInfo(conj))
    }
    map.apply(conj).addRelation(category)
  }

  def getConjunction(child: LexicalChainInstanceNode): String = {
    val lexemes = child.getLexemes
    if(lexemes.size > 1 && lexemes.head.category == "C") {
      lexemes.head.word
    }
    else
      null
  }

  def findChainNode(startOffset: Int): LexicalChainInstanceNode = {
    var targetNode: LexicalChainInstanceNode = null
    var i = 0
    while(i < getChildCount && targetNode == null) {
      val child = getChildAt(i)
      child match {
        case chainChild: LexicalChainInstanceNode =>
          if (chainChild.startOffset == startOffset)
            targetNode = chainChild
        case _ =>
          val relationChild = child.asInstanceOf[RelationNode]
          targetNode = relationChild.findChainNode(startOffset)
      }
      i += 1
    }
    targetNode
  }

  def extractAllChainInstanceNodes(list: mutable.MutableList[LexicalChainInstanceNode]) {
    for(i <- 0 until getChildCount) {
      val child = getChildAt(i)
      child match {
        case node: LexicalChainInstanceNode => list += node
        case _ => child.asInstanceOf[RelationNode].extractAllChainInstanceNodes(list)
      }
    }
  }
  
  def isComplete: Boolean = {
    if(getChildCount < CAPACITY)
      false
    else {
      var complete = true
      var i = 0
      while(i < getChildCount && complete) {
        complete = getChildAt(i).asInstanceOf[RSTNode].isComplete
        i += 1
      }
      complete
    }
  }
  
  def isLegal: Boolean = {
    var legal = true
    var i = 0
    while(i < getChildCount && legal) {
      legal = getChildAt(i).asInstanceOf[RSTNode].isLegal
      i += 1
    }
    legal
  }
  
  def adjust(span: TextSpan) {
    if(startOffset > span.startOffset)
      startOffset -= span.length
    for(i <- 0 until getChildCount)
      getChildAt(i).asInstanceOf[RSTNode].adjust(span)
  }
  
  def getInfo: Info = {
    val info = Info(1, 0, 1, 0)
    var maxHeight = -1
    for(i <- 0 until getChildCount) {
      val childInfo = getChildAt(i).asInstanceOf[RSTNode].getInfo
      info += childInfo
      maxHeight = Math.max(maxHeight, childInfo.height)
    }
    info.height += maxHeight
    if(info.numberOfEleRelation == 0)
      info.numberOfEleRelation = 1
    info
  }
  
  def standardize() {
    for(i <- 0 until getChildCount) {
      getChildAt(i).asInstanceOf[RSTNode].standardize()
    }
  }
  
  def searchRelationNode(category: String, list: mutable.MutableList[RelationNode]){
    for(i <- 0 until getChildCount) {
      val child = getChildAt(i)
      child match {
        case node: RelationNode =>
          if (node.category.toLowerCase.startsWith(category.toLowerCase)) {
            list += node
          }
          node.searchRelationNode(category, list)
        case _ =>
      }
    }
  }
  
  def getInfoString = s"$category[${model.fileName}:$startOffset]"
}

class LexicalChainInstanceNode(initRole: Int, initStartOffset: Int, var endOffset: Int, _model: RSTTreeModel, val fileName: String) extends RSTNode {
  
  role = initRole
  startOffset = initStartOffset
  model = _model
  
  var name: String = ""
  
  var raw = false
  
  updateName()
  
  def getLexemes: mutable.MutableList[Lexeme] = Resource.getRSTDocument(fileName).getFilteredLexemeList(startOffset, endOffset)
  
  def getLexemesString: String = getLexemes.mkString("{", ",", "}")
  
  def updateName() {
//    val newName = s"[${displayRole(role)}]:${startOffset}-${endOffset}:${getLexemes.mkString("{", ",", "}")}"
    val newName = s"$startOffset-$endOffset:${getLexemes.mkString("{", ",", "}")}"
    if(name != newName) {
      name = newName
      setUserObject(name)
      model.nodeChanged(this)
    }
  }
  
  def canAddChild(incoming: RSTNode) = false
  
  def delete() {
    val parent = getParent.asInstanceOf[RSTNode]
    parent.removeChild(this)
    parent.updateName()
    Resource.editor.document.demarkSpan(startOffset, endOffset)
    if(!raw)
      ActionEventDispatcher.fireActionEvent(new ActionEvent(this, 0, Command.LEXICAL_CHAIN_INSTANCE_DELETED))
  }
  
  def highlight() {
    Resource.editor.setCaretPosition(startOffset)
    role match {
      case NUCLEUS_ROLE => Resource.editor.document.highlightSpanAsNucleus(startOffset, endOffset)
      case SATELLITE_ROLE => Resource.editor.document.highlightSpanAsSatellite(startOffset, endOffset)
      case UNDEFINED_ROLE => Resource.editor.document.highlightSpanAsUndefined(startOffset, endOffset)
    }
  }
  
  def dehighlight() {
    Resource.editor.document.markSpan(startOffset, endOffset)
  }
  
  def highlightAsNucleus() {
    Resource.editor.document.highlightSpanAsNucleus(startOffset, endOffset)
  }
  
  def highlightAsSatellite() {
    Resource.editor.document.highlightSpanAsSatellite(startOffset, endOffset)
  }
  
  def refresh() {
    Resource.editor.document.markSpan(startOffset, endOffset)
    updateName()
  }
  
  def isSpanMarked(spanStartOffset: Int, spanEndOffset: Int): Boolean = !(spanEndOffset <= startOffset || endOffset <= spanStartOffset)
  
  def generateRule = s"<${Resource.patternTreeModel.getPatternName(this)}>:[${Resource.getRSTDocument(fileName).getFilteredLexemeList(startOffset, endOffset).mkString("{", ",", "}")}]"
  
  def generateRuleHead = s"[<${Resource.patternTreeModel.getPatternName(this)}>:$generateIOString]"
  
  def exportRule(out: PrintWriter) { out.println(generateRule) }
  
  def exportConjunctions(map: mutable.HashMap[String, RelationInfo]): Unit = {}
  
  def isComplete = true
  
  def isLegal: Boolean = {
    var headCount = 0
    val list = getLexemes
    list.foreach(lex => {
      if(lex.category == "H")
        headCount += 1
    })
    headCount == 1
  }
  
  def adjust(span: TextSpan) {
    if(startOffset > span.startOffset) {
      startOffset -= span.length
      endOffset -= span.length
      updateName()
    }
  }
  
  def getInfo = Info(0, 1, 0, 0)
  
  def standardize() {
    val doc = Resource.getRSTDocument(fileName)
    val standard = doc.getStandardSpan(startOffset, endOffset)
    if(standard != null && (standard._1 != startOffset || standard._2 != endOffset)) {
      Resource.patternTreeModel.getLexicalChainNode(this).updatePosition(this, standard._1, standard._2)
      doc.demarkSpan(startOffset, endOffset)
      startOffset = standard._1
      endOffset = standard._2
      doc.markSpan(startOffset, endOffset)
      updateName()
    }
  }
}

class RawNode(fileName: String) extends RSTNode {
  
  var name = ""
  
  def setModel(model: RSTTreeModel) {
    this.model = model
    updateName()
  }
  
  def updateName() {
    val newName = s"Raw[$getChildCount]"
    if(name != newName) {
      name = newName
      setUserObject(newName)
    }
    model.nodeChanged(this)
  }
  
  def canAddChild(incoming: RSTNode): Boolean = incoming.getParent != this && incoming.isInstanceOf[LexicalChainInstanceNode]
  
  override def addChild(incoming: RSTNode) {
    if(canAddChild(incoming)) {
      incoming.role = incoming.UNDEFINED_ROLE
      addLexicalChainInstanceNode(incoming.asInstanceOf[LexicalChainInstanceNode])
      ActionEventDispatcher.fireActionEvent(new ActionEvent(incoming, 0, Command.LEXICAL_CHAIN_INSTANCE_DELETED))
    }
  }
  
  override def removeChild(leaving: RSTNode) {
    val node = leaving.asInstanceOf[LexicalChainInstanceNode]
    Resource.editor.document.demarkSpan(node.startOffset, node.endOffset)
    model.removeNodeFromParent(leaving)
    updateName()
  }
  
  private def addLexicalChainInstanceNode(node: LexicalChainInstanceNode) {
    var i = 0
    var found = false
    node.raw = true
    while(i < getChildCount && !found) {
      val iter = getChildAt(i).asInstanceOf[LexicalChainInstanceNode]
      if(node.startOffset < iter.startOffset)
        found = true
      else
        i += 1
    }
    model.insertNodeInto(node, this, i)
    updateName()
  }

  def isSpanMarked(spanStartOffset: Int, spanEndOffset: Int): Boolean = {
    var marked = false
    var i = 0
    while(!marked && i < getChildCount) {
      marked = getChildAt(i).asInstanceOf[RSTNode].isSpanMarked(spanStartOffset, spanEndOffset)
      i += 1
    }
    marked
  }
  
  def delete() {
    val childCount = getChildCount
    for(_ <- 0 until childCount)
      removeChild(getChildAt(0).asInstanceOf[RSTNode])
  }
  
  def generate() {
    val list = Resource.getRSTDocument(fileName).rawLexicalChainList
    list.foreach(raw => {
      if(!model.isSpanMarked(raw.startOffset, raw.endOffset)) {
        addLexicalChainInstanceNode(new LexicalChainInstanceNode(-1, raw.startOffset, raw.endOffset, model, fileName))
      }
    })
    updateName()
  }

  def highlight() {
    for(i <- 0 until getChildCount)
      getChildAt(i).asInstanceOf[RSTNode].highlight()
    
    if(getChildCount > 0)
      Resource.editor.setCaretPosition(getChildAt(0).asInstanceOf[RSTNode].startOffset)
  }
  
  def dehighlight() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].dehighlight() }
  
  def refresh() { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].refresh() }

  def highlightAsNucleus(): Unit = {}

  def highlightAsSatellite(): Unit = {}

  def isComplete = true
  
  def isLegal: Boolean = getChildCount == 0

  def exportRule(out: PrintWriter): Unit = {}
  
  def exportConjunctions(map: mutable.HashMap[String, RelationInfo]): Unit = {}

  def generateRule: String = ""

  def generateRuleHead: String = ""
  
  def adjust(span: TextSpan) { for(i <- 0 until getChildCount) getChildAt(i).asInstanceOf[RSTNode].adjust(span) }
  
  def getInfo: Info = dummyInfo
  
  def standardize() {}
}
