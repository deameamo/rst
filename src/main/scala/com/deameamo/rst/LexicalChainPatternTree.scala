package com.deameamo.rst

import java.awt.{Color, Component, Insets, Point}
import java.awt.dnd.Autoscroll
import java.awt.event._
import javax.swing._
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeCellRenderer, TreePath}

import com.deameamo.swingx.MenuBuilder
import com.deameamo.event.ActionEventDispatcher

import scala.collection.mutable

class LexicalChainPatternTree extends JTree with ActionListener with MouseListener with Autoscroll with KeyListener {

  val ADD_PATTERN = "ADD_PATTERN"
  val DELETE_PATTERN = "DELETE_PATTERN"
  val CHANGE_PATTERN_NAME = "CHANGE_PATTERN_NAME"
  val CUT = "CUT"
  val PASTE = "PASTE"
  val DELETE_CHAIN = "DELETE_CHAIN"
  
  val model: LexicalChainPatternTreeModel = Resource.patternTreeModel
  setModel(model)
  
  var toBeCut: LexicalChainNode = _
  
  val mb = new MenuBuilder("com.deameamo.rst.PatternTreePopupMenu")
  val rootPopupMenu = new JPopupMenu
  rootPopupMenu.add(mb.createMenuItem("AddPattern", this, ADD_PATTERN))
  val pasteItemOfRootMenu: JMenuItem = mb.createMenuItem("Paste", this, PASTE)
  rootPopupMenu.addSeparator()
  rootPopupMenu.add(pasteItemOfRootMenu)
  val patternPopupMenu = new JPopupMenu
  patternPopupMenu.add(mb.createMenuItem("AddPattern", this, ADD_PATTERN))
  patternPopupMenu.add(mb.createMenuItem("ChangePatternName", this, CHANGE_PATTERN_NAME))
  patternPopupMenu.add(mb.createMenuItem("Delete", this, DELETE_PATTERN))
  val pasteItemOfPatternMenu: JMenuItem = mb.createMenuItem("Paste", this, PASTE)
  patternPopupMenu.addSeparator()
  patternPopupMenu.add(pasteItemOfPatternMenu)
  val chainPopupMenu = new JPopupMenu
  val cutItem: JMenuItem = mb.createMenuItem("Cut", this, CUT)
  chainPopupMenu.add(cutItem)
  chainPopupMenu.add(mb.createMenuItem("Delete", this, DELETE_CHAIN))
  
  addMouseListener(this)
  addKeyListener(this)
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.SHOW_CONTRADICTS)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_INSTANCE_SELECTED)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_INSTANCE_VIEWED)
  ActionEventDispatcher.addActionListener(this, Command.TEXT_SPAN_DELETED)
  
  val renderer = new PatternTreeCellRenderer
  
  val normalColor: Color = renderer.getBackgroundSelectionColor
  val alertColor: Color = Color.RED
  setCellRenderer(renderer)
  
  var legalized = true
  def notifyDataContradicted(): Unit = {
    legalized = false
    renderer.setBackgroundSelectionColor(alertColor)
  }
  def notifyDataLegalized(): Unit = {
    legalized = true
    renderer.setBackgroundSelectionColor(normalColor)
  }
  
  var selectedLexicalChainNode: LexicalChainNode = _
  
  def showContradicts(nodes: Iterable[DefaultMutableTreeNode], selected: LexicalChainNode) {
    if(nodes.isEmpty)
      notifyDataLegalized()
    else {
      notifyDataContradicted()
      val paths = new mutable.MutableList[TreePath]
      if(selected != null)
        paths += new TreePath(model.getPathToRoot(selected).asInstanceOf[Array[Object]])
      for(node <- nodes) {
        if(node != selected)
          paths += new TreePath(model.getPathToRoot(node).asInstanceOf[Array[Object]])
      }
      setSelectionPaths(paths.toArray)
    }
  }
  
  def actionPerformed(event: ActionEvent) {
    val selected = getSelectedNode
    event.getActionCommand match {
      case ADD_PATTERN =>
        val name = JOptionPane.showInputDialog(this, "Please enter the pattern name:", "Create Pattern", JOptionPane.QUESTION_MESSAGE)
        if(name.length > 0) {
          selected.asInstanceOf[LexicalChainPatternNode].addPatternNode(name)
          model.notifyDataChanged()
        }
      case CHANGE_PATTERN_NAME =>
        val name = JOptionPane.showInputDialog(this, "Please enter the NEW pattern name:", "Change Pattern Name", JOptionPane.QUESTION_MESSAGE)
        if(name.length > 0) {
          selected.asInstanceOf[LexicalChainPatternNode].changePatternName(name)
          model.notifyDataChanged()
        }
      case DELETE_PATTERN =>
        selected.asInstanceOf[LexicalChainPatternNode].delete()
        model.notifyDataChanged()
        model.checkForContradiction()
      case DELETE_CHAIN => deleteLexicalChainNode(selected.asInstanceOf[LexicalChainNode])
      case CUT => cutLexicalChainNode(selected.asInstanceOf[LexicalChainNode])
      case PASTE => pasteLexicalChainNode(selected.asInstanceOf[LexicalChainPatternNode])
      case Command.RESOURCE_LOADED => setRowHeight(Resource.fontSize + 4)
      case Command.SHOW_CONTRADICTS =>
        val contradicts = event.getSource.asInstanceOf[mutable.MutableList[DefaultMutableTreeNode]]
        showContradicts(contradicts, null)
      case Command.LEXICAL_CHAIN_INSTANCE_SELECTED =>
        val instance = event.getSource.asInstanceOf[LexicalChainInstanceNode]
        val node = model.getLexicalChainNode(instance)
        if(node != null) {
          val path = new TreePath(model.getPathToRoot(node.getParent).asInstanceOf[Array[Object]])
          setSelectionPath(path)
          scrollRowToVisible(getRowForPath(path))
          selectNode(node, node.getPositionIndex(instance))
        }
      case Command.LEXICAL_CHAIN_INSTANCE_VIEWED =>
        val selected = event.getSource.asInstanceOf[LexicalChainPosition].node
        if(selected != selectedLexicalChainNode) {
          val path = new TreePath(model.getPathToRoot(selected.getParent).asInstanceOf[Array[Object]])
          setSelectionPath(path)
          scrollRowToVisible(getRowForPath(path))
        }
        selectedLexicalChainNode = selected
      case Command.TEXT_SPAN_DELETED =>
        val span = event.getSource.asInstanceOf[TextSpan]
        model.adjust(span)
        ActionEventDispatcher.fireActionEvent(new ActionEvent(span, 0, Command.PATTERN_TREE_ADJUSTED))
    }
  }
  
  def cutLexicalChainNode(node: LexicalChainNode) {
    toBeCut = node
  }
  
  def pasteLexicalChainNode(parent: LexicalChainPatternNode) {
    if(toBeCut == null)
      return
    model.removeLexicalChainNode(toBeCut)
    parent.addChainNode(toBeCut)
    toBeCut = null
    model.checkForContradiction()
    model.notifyDataChanged()
  }
  
  def deleteLexicalChainNode(node: LexicalChainNode) {
    model.removeLexicalChainNode(node)
    model.checkForContradiction()
    model.notifyDataChanged()
  }
  
  def mousePressed(e: MouseEvent): Unit = {
    val selected = getSelectedNode
    if(selected == null)
      return
    if(e.getButton == MouseEvent.BUTTON3) {
      if(selected.isInstanceOf[LexicalChainPatternNode]) {
        if(selected == model.getRoot) {
          if(toBeCut != null) {
            pasteItemOfRootMenu.setEnabled(true)
          }
          else {
            pasteItemOfRootMenu.setEnabled(false)
          }
          setComponentPopupMenu(rootPopupMenu)
        }
        else {
          if(toBeCut != null) {
            pasteItemOfPatternMenu.setEnabled(true)
          }
          else {
            pasteItemOfPatternMenu.setEnabled(false)
          }
          setComponentPopupMenu(patternPopupMenu)
        }
      }
      else {
        setComponentPopupMenu(chainPopupMenu)
      }
    }
    else if(e.getButton == MouseEvent.BUTTON1) {
      selectNode(selected, 0)
    }
  }
  
  def selectNode(selected: Object, index: Int) {
    selected match {
      case node: LexicalChainNode =>
        selectedLexicalChainNode = node
        val event = new ActionEvent(selected, index, Command.LEXICAL_CHAIN_SELECTED)
        ActionEventDispatcher.fireActionEvent(event)
        if (!legalized)
          showContradicts(model.getContradictingNodes(node), node)
      case _ =>
        val patternNode = selected.asInstanceOf[LexicalChainPatternNode]
        if (patternNode == model.getRoot)
          showContradicts(model.contradicts, null)
    }
  }
  
  def getSelectedNode: Object = {
    if(model != null) {
      val path = getSelectionPath
      if(path != null)
        path.getLastPathComponent
      else
        model.getRoot
    }
    else
      null
  }

  def mouseClicked(e: MouseEvent): Unit = {}
  def mouseEntered(e: MouseEvent): Unit = {}
  def mouseExited(e: MouseEvent): Unit = {}
  def mouseReleased(e: MouseEvent): Unit = {}
  
  def keyReleased(e: KeyEvent) {
    e.getKeyCode match {
      case KeyEvent.VK_DELETE =>
        val node = getSelectedNode
        if(node != null && node.isInstanceOf[LexicalChainNode])
          deleteLexicalChainNode(node.asInstanceOf[LexicalChainNode])
      case KeyEvent.VK_X =>
        if(e.isControlDown) {
          val node = getSelectedNode
          if(node != null && node.isInstanceOf[LexicalChainNode])
            cutLexicalChainNode(node.asInstanceOf[LexicalChainNode])
        }
      case KeyEvent.VK_V =>
        if(toBeCut != null && e.isControlDown) {
          val node = getSelectedNode
          if(node != null && node.isInstanceOf[LexicalChainPatternNode])
            pasteLexicalChainNode(node.asInstanceOf[LexicalChainPatternNode])
        }
      case _ =>
    }
  }
  
  def keyPressed(e: KeyEvent) {}
  def keyTyped(e: KeyEvent) {}
  
  val margin = 12
  
  def autoscroll(p: Point) {
    val realRow = getRowForLocation(p.x, p.y)
    val outer = getBounds()
    val newRow = {
      if(p.y + outer.y < margin) {
        if(realRow < 1) 0 else realRow - 1
      }
      else {
        if(realRow < getRowCount - 1) realRow + 1 else realRow
      }
    }
    scrollRowToVisible(newRow)
  }
  
  def getAutoscrollInsets: Insets = {
    val outer = getBounds()
    val inner = getParent.getBounds()
    new Insets(inner.y - outer.y + margin, inner.x - outer.x + margin,
        outer.height - inner.height - inner.y + outer.y + margin,
        outer.width - inner.width - inner.x + outer.x + margin)
  }
}

class PatternTreeCellRenderer extends DefaultTreeCellRenderer {
  
  val patternIcon = new ImageIcon("icons/pattern.png")
  val patternWarningIcon = new ImageIcon("icons/pattern_warning.png")
  val chainIcon = new ImageIcon("icons/chain_class.png")
  
  override def getTreeCellRendererComponent(tree: JTree, value: Object, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean): Component = {
    super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    val path = tree.getPathForRow(row)
    if(path != null) {
      val icon = {
        val node = path.getLastPathComponent
        node match {
          case node1: LexicalChainPatternNode =>
            if (node1.isLegal)
              patternIcon
            else
              patternWarningIcon
          case _ =>
            chainIcon
        }
      }
      setIcon(icon)
      setFont(Resource.defaultFont)
    }
    this
  }
}