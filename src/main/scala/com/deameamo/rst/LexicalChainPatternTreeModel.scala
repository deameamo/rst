package com.deameamo.rst

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeModel}

import com.deameamo.event.ActionEventDispatcher
import com.deameamo.util.ArrayList
import org.w3c.dom.Element

import scala.collection.mutable

class LexicalChainPatternTreeModel(root: LexicalChainPatternNode = new LexicalChainPatternNode("All"))
    extends DefaultTreeModel(root: LexicalChainPatternNode) with ActionListener {

  root.model = this
  val nodeMap = new mutable.HashMap[String, mutable.HashSet[LexicalChainNode]]
  val contradicts = new mutable.MutableList[DefaultMutableTreeNode]
  
  var dataChanged = false
  var autoChecking = false
  
  ActionEventDispatcher.addActionListener(this, Command.SYNCHED)
  ActionEventDispatcher.addActionListener(this, Command.AUTO_CHECK_CHANGED)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_INSTANCE_ADDED)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_INSTANCE_DELETED)
  ActionEventDispatcher.addActionListener(this, Command.DELETE_POSITION)
  
  def load() {
    nodeMap.clear
    root.clear()
    val document = Resource.loadPatternTreeDocument
    if(document == null) {
      val fileNameList = Resource.getTreeFileNameList
      for(fileName <- fileNameList) {
        val model = Resource.getTreeModel(fileName)
        val chainNodes = model.getAllChainInstanceNodes
        for(chainNode <- chainNodes) {
          addInstanceNode(chainNode)
        }
      }
    }
    else {
      val docEle = document.getDocumentElement
      val childNodes = docEle.getChildNodes
      for(i <- 0 until childNodes.getLength) {
        val child = childNodes.item(i).asInstanceOf[Element]
        child.getNodeName match {
          case "pattern" => root.addPatternNode(buildPatternNode(child))
          case "chain" => root.addChainNode(buildChainNode(root, child))
        }
      }
      checkForConsistency()
      checkForContradiction()
    }
  }

  private def checkForConsistency() {
    for(nodes <- nodeMap.values) {
      for(node <- nodes) {
        for(pos <- node.positions) {
          val tree = Resource.getTreeModel(pos.fileName)
          val path = tree.getTreePath(pos.startOffset)
          if(path == null) {
            println(s"${pos.fileName}:${pos.startOffset}: no instance found: parent:${node.getParent.asInstanceOf[LexicalChainPatternNode].patternName}, lexemes: ${node.lexemesString}")
          }
          else {
            val instance = path.getLastPathComponent.asInstanceOf[LexicalChainInstanceNode]
            if(instance.startOffset != pos.startOffset) {
              println(s"startOffset inconsistent: ${pos.fileName}:${pos.startOffset}")
            }
            if(instance.endOffset != pos.endOffset) {
              pos.endOffset = instance.endOffset
              println(s"${pos.fileName}:${pos.startOffset}: endOffset inconsistent: parent:${node.getParent.asInstanceOf[LexicalChainPatternNode].patternName}, lexemes: ${node.lexemesString}")
            }
          }
        }
      }
    }

    println(s"====================================================================================")
    val fileNameList = Resource.doneList
    for(fileName <- fileNameList) {
      val model = Resource.getTreeModel(fileName)
      val chainNodes = model.getAllChainInstanceNodes
      for(instance <- chainNodes) {
        val node = getLexicalChainNode(instance)
        if(node == null) {
          addInstanceNode(instance)
        }
      }
    }
  }

  def search(keys: Array[String]): mutable.MutableList[LexicalChainPosition] = {
    val list = new mutable.MutableList[LexicalChainPosition]
    for(nodes <- nodeMap.values) {
      for(node <- nodes) {
        if(node.contains(keys))
          list ++= node.positions
      }
    }
    list.sortWith((a, b) => {
      val fileNamePre = a.fileName.compareTo(b.fileName)
      if(fileNamePre < 0)
        true
      else if(fileNamePre > 0)
        false
      else {
        if(a.startOffset < b.startOffset)
          true
        else
          false
      }
    })
  }

  private def buildPatternNode(ele: Element): LexicalChainPatternNode = {
    val node = new LexicalChainPatternNode(ele.getAttribute("name"), this)
    val childNodes = ele.getChildNodes
    for(i <- 0 until childNodes.getLength) {
      val child = childNodes.item(i).asInstanceOf[Element]
      child.getNodeName match {
          case "pattern" => node.addPatternNode(buildPatternNode(child))
          case "chain" => node.addChainNode(buildChainNode(node, child))
      }
    }
//    if(node.getChildCount == 0)
//      null
//    else
      node
  }

  private def buildChainNode(parent: LexicalChainPatternNode, ele: Element): LexicalChainNode = {
//    val lexemes = new MutableList[Lexeme]
//    if(ele.getAttribute("lexemes").length > 0) {
//      val shards = ele.getAttribute("lexemes").split(",")
//      for(shard <- shards) {
//        val subShards = shard.split("""\|""")
//        lexemes += new Lexeme(subShards.head, subShards.last)
//      }
//    }
//    val node = new LexicalChainNode(lexemes, this)

    val childNodes = ele.getChildNodes
    def getLexemeList(index: Int) = {
      val child = childNodes.item(index).asInstanceOf[Element]
      val document = Resource.getRSTDocument(child.getAttribute("fileName"))
      document.getFilteredLexemeList(child.getAttribute("startOffset").toInt, child.getAttribute("endOffset").toInt)
    }
    val nodeLexemeList = getLexemeList(0)
    val node = new LexicalChainNode(nodeLexemeList, this)
    for(i <- 0 until childNodes.getLength) {
      val child = childNodes.item(i).asInstanceOf[Element]
      val fileName = child.getAttribute("fileName")
      if(Resource.doneList.contains(fileName)) {
        val childLexemeList = getLexemeList(i)
        val startOffset = child.getAttribute("startOffset").toInt
        val endOffset = child.getAttribute("endOffset").toInt
        if(childLexemeList.mkString("{", ",", "}") == nodeLexemeList.mkString("{", ",", "}")) {
          node.addPosition(fileName, startOffset, endOffset)
        }
        else {
          val newNode = new LexicalChainNode(childLexemeList, this)
          newNode.addPosition(fileName, startOffset, endOffset)
          parent.addChainNode(newNode)
        }
      }
    }
//    println(s"${node.lexemesString}: ${node.getChildCount}")
    if(node.positions.isEmpty)
      null
    else
      node
  }

  def getContradictingNodes(node: LexicalChainNode): Iterable[LexicalChainNode] = {
    nodeMap(node.lexemesString)
  }

  def getPatternName(instance: LexicalChainInstanceNode): String = {
    val node = getLexicalChainNode(instance)
    node.getParent.asInstanceOf[LexicalChainPatternNode].patternName
  }

  def getLexicalChainNode(instance: LexicalChainInstanceNode): LexicalChainNode = {
    val nodes = nodeMap.get(instance.getLexemesString)
    nodes match {
      case None =>
        val msg = s"${instance.fileName}:${instance.startOffset}: no lexical chain node found: lexemes: ${instance.getLexemesString}"
        println(msg)
        null
      case Some(_) =>
        var target: LexicalChainNode = null
        for(node <- nodes.get) {
          if(target == null && node.containsPosition(instance)) {
            target = node
          }
        }
        target
    }
  }

  def removeLexicalChainNode(node: LexicalChainNode) {
    node.getParent.asInstanceOf[LexicalChainPatternNode].removeChainNode(node)
  }

  def addInstanceNode(instance: LexicalChainInstanceNode) {
    val node = new LexicalChainNode(instance.getLexemes, this)
    node.addPosition(instance)
    if(!autoChecking) {
      root.addChainNode(node)
    }
    else {
      val joiningNode = addLexicalChainNodeIntoNodeMap(node, null)
      if(joiningNode == null) {
        root.addChainNode(node)
        val msg = s"""Adding ${node.lexemesString} to "All""""
        ActionEventDispatcher.fireActionEvent(new ActionEvent(msg, 0, Command.SET_STATUS))
      }
      else {
        joiningNode.addPosition(instance)
        val msg = s"""Adding ${node.lexemesString} to "${getPatternNameChain(joiningNode)}""""
        ActionEventDispatcher.fireActionEvent(new ActionEvent(msg, 0, Command.SET_STATUS))
      }
    }
  }

  private def getPatternNameChain(node: LexicalChainNode): String = {
    val path = getPathToRoot(node)
    val list = new mutable.MutableList[String]
    for(i <- 0 until path.size - 1) {
      list += path.apply(i).asInstanceOf[LexicalChainPatternNode].patternName
    }
    list.mkString("-")
  }

  def removeInstanceNode(instance: LexicalChainInstanceNode) {
    val node = getLexicalChainNode(instance)
    if(node != null) {
      node.removePosition(instance)
      if(node.positions.isEmpty)
        node.getParent.asInstanceOf[LexicalChainPatternNode].removeChainNode(node)
    }
  }

  def removePosition(pos: LexicalChainPosition) {
    if(pos != null) {
      pos.node.removePosition(pos)
      if(pos.node.positions.isEmpty)
        pos.node.getParent.asInstanceOf[LexicalChainPatternNode].removeChainNode(pos.node)
    }
  }

  def removePosition(node: LexicalChainNode, index: Int) {
    if(node != null) {
      node.removePosition(index)
      if(node.positions.isEmpty)
        node.getParent.asInstanceOf[LexicalChainPatternNode].removeChainNode(node)
    }
  }

  def addLexicalChainNodeIntoNodeMap(node: LexicalChainNode, parent: LexicalChainPatternNode): LexicalChainNode = {
    if(!nodeMap.contains(node.lexemesString)) {
      val nodes = new mutable.HashSet[LexicalChainNode]
      nodes += node
      nodeMap.put(node.lexemesString, nodes)
      null
    }
    else {
      val existentNodes = nodeMap(node.lexemesString)
      if(parent == null) {
        if(existentNodes.size == 1)
          existentNodes.head
        else
          null
      }
      else {
        var incumbent: LexicalChainNode = null
        for(existentNode <- existentNodes) {
          if(incumbent == null && existentNode.getParent == parent)
            incumbent = existentNode
        }
        if(incumbent == null) existentNodes += node
        incumbent
      }
    }
  }

  def removeLexicalChainNodeFromNodeMap(node: LexicalChainNode) {
    if(nodeMap.contains(node.lexemesString)) {
      val nodes = nodeMap(node.lexemesString)
      nodes.remove(node)
      if(nodes.isEmpty)
        nodeMap.remove(node.lexemesString)
    }
  }

  def checkForContradiction() {
    contradicts.clear
    for(set <- nodeMap.values) {
      if(set.size > 1)
        contradicts ++= set
    }
    if(contradicts.nonEmpty) {
      contradicts += root
    }
    ActionEventDispatcher.fireActionEvent(new ActionEvent(contradicts, 0, Command.SHOW_CONTRADICTS))
  }

  def adjust(span: TextSpan) {
    for(map <- nodeMap.values) {
      for(node <- map)
        node.adjust(span)
    }
  }

  def actionPerformed(event: ActionEvent) {
    event.getActionCommand match {
      case Command.SYNCHED =>
        dataChanged = false
        root.synch()
        if(dataChanged)
          notifyDataChanged()
        checkForContradiction()
      case Command.AUTO_CHECK_CHANGED => autoChecking = event.getSource.asInstanceOf[Boolean]
      case Command.LEXICAL_CHAIN_INSTANCE_ADDED =>
        addInstanceNode(event.getSource.asInstanceOf[LexicalChainInstanceNode])
        notifyDataChanged()
        checkForContradiction()
      case Command.LEXICAL_CHAIN_INSTANCE_DELETED =>
        removeInstanceNode(event.getSource.asInstanceOf[LexicalChainInstanceNode])
        notifyDataChanged()
        checkForContradiction()
      case Command.DELETE_POSITION =>
        removePosition(event.getSource.asInstanceOf[LexicalChainPosition])
        notifyDataChanged()
        checkForContradiction()
    }
  }
  
  def notifyDataChanged(): Unit = ActionEventDispatcher.fireActionEvent(Command.PATTERN_TREE_DATA_CHANGED)
}

class LexicalChainPatternNode(var patternName: String, var model: LexicalChainPatternTreeModel) extends DefaultMutableTreeNode {
  
  def this(patternName: String) = this(patternName, null)
  
  var name = ""
  var patternNodeCount = 0
  
  val toBeAddedNodes = new mutable.MutableList[LexicalChainNode]
  val toBeRemovedNodes = new mutable.MutableList[LexicalChainNode]
  
  val synchResults = new mutable.MutableList[SynchResult]
  
  if(model != null)
    updateName()
  
  def changePatternName(newName: String) {
    if(newName != patternName) {
      patternName = newName
      updateName()
    }
  }
  
  def isLegal: Boolean = {
    if(patternNodeCount == 0)
      true
    else if(getChildCount == patternNodeCount)
      true
    else
      false
  }
  
  def clear() {
    for(i <- 0 until patternNodeCount)
      getChildAt(i).asInstanceOf[LexicalChainPatternNode].clear()
    
    val count = getChildCount
    for(_ <- 0 until count)
      model.removeNodeFromParent(getChildAt(0).asInstanceOf[DefaultMutableTreeNode])
      
    patternNodeCount = 0
  }
  
  def synch() {
    synchResults.clear
    for(i <- patternNodeCount until getChildCount) {
      val chainNode = getChildAt(i).asInstanceOf[LexicalChainNode]
      synchResults += chainNode.synch
    }
    var offset = 0
    for(synchResult <- synchResults) {
      for(node <- synchResult.toBeAddedNodes) {
        model.dataChanged = true
        if(insertChainNode(node, synchResult.index + 1))
          offset += 1
      }
      if(synchResult.toBeRemovedNode != null) {
        offset -= 1
        removeChainNode(synchResult.toBeRemovedNode)
        model.dataChanged = true
      }
      else {
        if(synchResult.toBeAddedNodes.nonEmpty)
          synchResult.synchingNode.updateName()
      }
    }
    for(i <- 0 until patternNodeCount)
      getChildAt(i).asInstanceOf[LexicalChainPatternNode].synch()
  }
  
  def canAddChild(child: DefaultMutableTreeNode): Boolean = child.getParent != this && !model.getPathToRoot(this).contains(child)
  
  def addPatternNode(patternName: String) {
    if(getPatternNode(patternName) == null) {
      model.insertNodeInto(new LexicalChainPatternNode(patternName, model), this, patternNodeCount)
      patternNodeCount += 1
      updateName()
    }
  }
  
  def addPatternNode(node: LexicalChainPatternNode) {
    if(node == null)
      return
    val existent = getPatternNode(node.patternName)
    if(existent != null) {
      for(i <- 0 until node.getChildCount) {
        val child = node.getChildAt(i)
        child match {
          case patternNode: LexicalChainPatternNode =>
            existent.addPatternNode(patternNode)
          case _ =>
            existent.addChainNode(child.asInstanceOf[LexicalChainNode])
        }
      }
    }
    else {
      model.insertNodeInto(node, this, patternNodeCount)
      patternNodeCount += 1
    }
    updateName()
  }
  
  def removePatternNode(node: LexicalChainPatternNode) {
    model.removeNodeFromParent(node)
    patternNodeCount -= 1
    updateName()
  }
  
  def delete() {
    val root = model.getRoot.asInstanceOf[LexicalChainPatternNode]
    for(_ <- 0 until patternNodeCount) {
      val child = getChildAt(0).asInstanceOf[LexicalChainPatternNode]
      removePatternNode(child)
      root.addPatternNode(child)
    }
    val chainChildCount = getChildCount
    for(_ <- 0 until chainChildCount) {
      val child = getChildAt(0).asInstanceOf[LexicalChainNode]
      removeChainNode(child)
      root.addChainNode(child)
    }
    getParent.asInstanceOf[LexicalChainPatternNode].removePatternNode(this)
  }
  
  def getPatternNode(patternName: String): LexicalChainPatternNode = {
    var target: LexicalChainPatternNode = null
    var i = 0
    while(i < patternNodeCount && target == null) {
      if(getChildAt(i).asInstanceOf[LexicalChainPatternNode].patternName == patternName)
        target = getChildAt(i).asInstanceOf[LexicalChainPatternNode]
      i += 1
    }
    target
  }
  
  def insertChainNode(node: LexicalChainNode, index: Int): Boolean = {
    val incumbent = model.addLexicalChainNodeIntoNodeMap(node, this)
    if(incumbent == null) {
      model.insertNodeInto(node, this, index)
      updateName()
      true
    }
    else {
      incumbent.addPositions(node)
      false
    }
  }
  
  def addChainNode(node: LexicalChainNode) {
    if(node == null)
      return
    val incumbent = model.addLexicalChainNodeIntoNodeMap(node, this)
    if(incumbent == null) {
      model.insertNodeInto(node, this, getChildCount)
      updateName()
    }
    else {
      incumbent.addPositions(node)
    }
  }
  
  def removeChainNode(node: LexicalChainNode) {
    model.removeLexicalChainNodeFromNodeMap(node)
    model.removeNodeFromParent(node)
    updateName()
  }
  
  def updateName() {
    val newName = s"[$patternNodeCount:${getChildCount - patternNodeCount}]$patternName"
    if(name != newName) {
      name = newName
      setUserObject(name)
      model.nodeChanged(this)
    }
  }
}

class LexicalChainNode(val lexemes: mutable.MutableList[Lexeme], model: LexicalChainPatternTreeModel) extends DefaultMutableTreeNode {
  
  var name = ""
  val positions = new ArrayList[LexicalChainPosition]
  val positionSet = new mutable.HashSet[LexicalChainPosition]
  val lexemesString: String = lexemes.mkString("{", ",", "}")
  
  updateName()
  
  def updateName() {
    val newName = s"[${positions.size}]$lexemesString"
    if(name != newName) {
      name = newName
      setUserObject(name)
      model.nodeChanged(this)
    }
  }
  
  def contains(keys: Array[String]): Boolean = {
    var doesContain = true
    var i = 0
    while(i < keys.length && doesContain) {
      var j = 0
      var found = false
      while(j < lexemes.length && !found) {
        if(lexemes.get(j).get.word == keys.apply(i))
          found = true
        j += 1
      }
      doesContain = found
      i += 1
    }
    doesContain
  }
  
  def getPatternName: String = getParent.asInstanceOf[LexicalChainPatternNode].patternName
  
  def addPositions(another: LexicalChainNode) {
    for(position <- another.positions) {
      addPosition(position)
    }
    updateName()
  }
  
  def addPosition(node: LexicalChainInstanceNode) {
    addPosition(LexicalChainPosition(this, node.fileName, node.startOffset, node.endOffset))
  }
  
  def addPosition(fileName: String, startOffset: Int, endOffset: Int) {
    addPosition(LexicalChainPosition(this, fileName, startOffset, endOffset))
  }
  
  def addPosition(position: LexicalChainPosition) {
    position.node = this
    if(!positionSet.contains(position)) {
      positionSet += position
      positions += position
      updateName()
    }
    else {
      println(s"Trying to add a duplicate position: ${position.fileName}:${position.startOffset}-${position.endOffset}")
    }
  }
  
  def removePosition(instance: LexicalChainInstanceNode) {
//    val index = getPositionIndex(instance)
    val position = LexicalChainPosition(this, instance.fileName, instance.startOffset, instance.endOffset)
    positions.removeElem(position)
    positionSet.remove(position)
    updateName()
  }
  
  def removePosition(index: Int) {
    val position = positions.apply(index)
    positions.removeAtIndex(index)
    positionSet.remove(position)
    updateName()
  }
  
  def removePosition(pos: LexicalChainPosition) {
    positions.removeElem(pos)
    positionSet.remove(pos)
    updateName()
  }
  
  def updatePosition(instance: LexicalChainInstanceNode, newStartOffset: Int, newEndOffset: Int) {
    val position = positions.apply(getPositionIndex(instance))
    position.startOffset = newStartOffset
    position.endOffset = newEndOffset
    ActionEventDispatcher.fireActionEvent(Command.PATTERN_TREE_DATA_CHANGED)
  }
  
  def containsPosition(instance: LexicalChainInstanceNode): Boolean = positions.contains(LexicalChainPosition(this, instance.fileName, instance.startOffset, instance.endOffset))
  
  def getPositionIndex(instance: LexicalChainInstanceNode): Int = positions.indexOf(LexicalChainPosition(this, instance.fileName, instance.startOffset, instance.endOffset))
  
  def synch: SynchResult = {
    val toBeAddedNodes = new mutable.HashMap[mutable.MutableList[Lexeme], LexicalChainNode]
    val remainingPositions = new mutable.MutableList[LexicalChainPosition]
    for(position <- positions) {
      val posLexemes = Resource.getRSTDocument(position.fileName).getFilteredLexemeList(position.startOffset, position.endOffset)
      if(!Resource.areLexemesEqual(lexemes, posLexemes)) {
        if(toBeAddedNodes.contains(posLexemes)) {
          toBeAddedNodes(posLexemes).addPosition(position)
        }
        else {
          val toBeAddedNode = new LexicalChainNode(posLexemes, model)
          toBeAddedNode.addPosition(position)
          toBeAddedNodes.put(posLexemes, toBeAddedNode)
        }
      }
      else
        remainingPositions += position
    }
    positions.clear
    positions ++= remainingPositions
    positionSet.clear
    positionSet ++= positions
    val toBeRemovedNode = {
      if(positions.isEmpty)
        this
      else
        null
    }
    SynchResult(this, getParent.getIndex(this), toBeRemovedNode, toBeAddedNodes.values)
  }
  
  def adjust(span: TextSpan) {
    for(position <- positions) {
      if(position.fileName == span.fileName && position.startOffset > span.startOffset) {
        position.startOffset -= span.length
        position.endOffset -= span.length
      }
    }
  }
}

case class LexicalChainPosition(var node: LexicalChainNode, fileName: String, var startOffset: Int, var endOffset: Int) {
  
  override def toString = s"$fileName:$startOffset-$endOffset(${node.getPatternName})"
}

case class SynchResult(synchingNode: LexicalChainNode, index: Int, toBeRemovedNode: LexicalChainNode, toBeAddedNodes: Iterable[LexicalChainNode])
