package com.deameamo.rst

import java.awt.Component
import java.awt.event._
import javax.swing._
import javax.swing.tree.{DefaultTreeCellRenderer, TreePath}

import com.deameamo.event.{ActionEventDispatcher, KeyAdapter}
import com.deameamo.swingx.MenuBuilder

import scala.collection.mutable

class RSTTree extends JTree with ActionListener with MouseListener with KeyAdapter {
  
  val ADD_ANALYSIS = "a_Analysis"
  val ADD_ANTITHESIS = "a_Antithesis"
  val ADD_BACKGROUND = "a_Background"
  val ADD_CONCESSION = "a_Concession"
  val ADD_EMPHASIS = "a_Emphasis"
  val ADD_ENABLEMENT = "a_Enablement"
  val ADD_EVIDENCE = "a_Evidence"
  val ADD_JUSTIFY = "a_Justify"
  val ADD_MOTIVATION = "a_Motivation"
  val ADD_PREPARATION = "a_Preparation"
  val ADD_RESTATEMENT = "a_Restatement"
  val ADD_SUMMARY = "a_Summary"
  val ADD_CIRCUMSTANCE = "a_Circumstance"
  val ADD_CONDITION = "a_Condition"
  val ADD_ELABORATION = "a_Elaboration"
  val ADD_EVALUATION = "a_Evaluation"
  val ADD_INTERPRETATION = "a_Interpretation"
  val ADD_MEANS = "a_Means"
  val ADD_NON_VOLITIONAL_CAUSE = "a_Non-volitional Cause"
  val ADD_NON_VOLITIONAL_RESULT = "a_Non-volitional Result"
  val ADD_OTHERWISE = "a_Otherwise"
  val ADD_PURPOSE = "a_Purpose"
  val ADD_SOLUTIONHOOD = "a_Solutionhood"
  val ADD_UNCONDITIONAL = "a_Unconditional"
  val ADD_UNLESS = "a_Unless"
  val ADD_VOLITIONAL_CAUSE = "a_Volitional Cause"
  val ADD_VOLITIONAL_RESULT = "a_Volitional Result"
  val ADD_CONJUNCTION = "a_Conjunction"
  val ADD_CONTRAST = "a_Contrast"
  val ADD_DISJUNCTION = "a_Disjunction"
  val ADD_JOINT = "a_Joint"
  val ADD_LIST = "a_List"
  val ADD_MULTINUCLEAR_RESTATEMENT = "a_Multinuclear Restatement"
  val ADD_SEQUENCE = "a_Sequence"

  val CHANGE_TO_ANALYSIS = "c_Analysis"
  val CHANGE_TO_ANTITHESIS = "c_Antithesis"
  val CHANGE_TO_BACKGROUND = "c_Background"
  val CHANGE_TO_CONCESSION = "c_Concession"
  val CHANGE_TO_EMPHASIS = "c_Emphasis"
  val CHANGE_TO_ENABLEMENT = "c_Enablement"
  val CHANGE_TO_EVIDENCE = "c_Evidence"
  val CHANGE_TO_JUSTIFY = "c_Justify"
  val CHANGE_TO_MOTIVATION = "c_Motivation"
  val CHANGE_TO_PREPARATION = "c_Preparation"
  val CHANGE_TO_RESTATEMENT = "c_Restatement"
  val CHANGE_TO_SUMMARY = "c_Summary"
  val CHANGE_TO_CIRCUMSTANCE = "c_Circumstance"
  val CHANGE_TO_CONDITION = "c_Condition"
  val CHANGE_TO_ELABORATION = "c_Elaboration"
  val CHANGE_TO_EVALUATION = "c_Evaluation"
  val CHANGE_TO_INTERPRETATION = "c_Interpretation"
  val CHANGE_TO_MEANS = "c_Means"
  val CHANGE_TO_NON_VOLITIONAL_CAUSE = "c_Non-volitional Cause"
  val CHANGE_TO_NON_VOLITIONAL_RESULT = "c_Non-volitional Result"
  val CHANGE_TO_OTHERWISE = "c_Otherwise"
  val CHANGE_TO_PURPOSE = "c_Purpose"
  val CHANGE_TO_SOLUTIONHOOD = "c_Solutionhood"
  val CHANGE_TO_UNCONDITIONAL = "c_Unconditional"
  val CHANGE_TO_UNLESS = "c_Unless"
  val CHANGE_TO_VOLITIONAL_CAUSE = "c_Volitional Cause"
  val CHANGE_TO_VOLITIONAL_RESULT = "c_Volitional Result"
  val CHANGE_TO_CONJUNCTION = "c_Conjunction"
  val CHANGE_TO_CONTRAST = "c_Contrast"
  val CHANGE_TO_DISJUNCTION = "c_Disjunction"
  val CHANGE_TO_JOINT = "c_Joint"
  val CHANGE_TO_LIST = "c_List"
  val CHANGE_TO_MULTINUCLEAR_RESTATEMENT = "c_Multinuclear Restatement"
  val CHANGE_TO_SEQUENCE = "c_Sequence"

  val CHANGE_TO_NUCLEUS = "1"
  val CHANGE_TO_SATELLITE = "2"
  val DELETE = "3"
  val EXPAND_ALL = "4"
  val DISBAND = "5"
  val GENERATE = "6"
  val CLEAR = "7"
  val ADD_TO_ROOT = "8"

  val mb = new MenuBuilder("com.deameamo.rst.TreePopupMenu")
  val rootMenu = new JPopupMenu()
  val addRepRelRootMenu: JMenu = mb.createMenu("AddRepRel")
  addRepRelRootMenu.add(mb.createMenuItem("Antithesis", this, ADD_ANTITHESIS))
  addRepRelRootMenu.add(mb.createMenuItem("Background", this, ADD_BACKGROUND))
  addRepRelRootMenu.add(mb.createMenuItem("Concession", this, ADD_CONCESSION))
  addRepRelRootMenu.add(mb.createMenuItem("Enablement", this, ADD_ENABLEMENT))
  addRepRelRootMenu.add(mb.createMenuItem("Evidence", this, ADD_EVIDENCE))
  addRepRelRootMenu.add(mb.createMenuItem("Justify", this, ADD_JUSTIFY))
  addRepRelRootMenu.add(mb.createMenuItem("Motivation", this, ADD_MOTIVATION))
  addRepRelRootMenu.add(mb.createMenuItem("Preparation", this, ADD_PREPARATION))
  addRepRelRootMenu.add(mb.createMenuItem("Restatement", this, ADD_RESTATEMENT))
  addRepRelRootMenu.add(mb.createMenuItem("Summary", this, ADD_SUMMARY))
  val addSMRelRootMenu: JMenu = mb.createMenu("AddSMRel")
  addSMRelRootMenu.add(mb.createMenuItem("Analysis", this, ADD_ANALYSIS))
  addSMRelRootMenu.add(mb.createMenuItem("Circumstance", this, ADD_CIRCUMSTANCE))
  addSMRelRootMenu.add(mb.createMenuItem("Condition", this, ADD_CONDITION))
  addSMRelRootMenu.add(mb.createMenuItem("Elaboration", this, ADD_ELABORATION))
  addSMRelRootMenu.add(mb.createMenuItem("Emphasis", this, ADD_EMPHASIS))
  addSMRelRootMenu.add(mb.createMenuItem("Evaluation", this, ADD_EVALUATION))
  addSMRelRootMenu.add(mb.createMenuItem("Interpretation", this, ADD_INTERPRETATION))
  addSMRelRootMenu.add(mb.createMenuItem("Means", this, ADD_MEANS))
  addSMRelRootMenu.add(mb.createMenuItem("Non-volitional_Cause", this, ADD_NON_VOLITIONAL_CAUSE))
  addSMRelRootMenu.add(mb.createMenuItem("Non-volitional_Result", this, ADD_NON_VOLITIONAL_RESULT))
  addSMRelRootMenu.add(mb.createMenuItem("Otherwise", this, ADD_OTHERWISE))
  addSMRelRootMenu.add(mb.createMenuItem("Purpose", this, ADD_PURPOSE))
  addSMRelRootMenu.add(mb.createMenuItem("Solutionhood", this, ADD_SOLUTIONHOOD))
  addSMRelRootMenu.add(mb.createMenuItem("Unconditional", this, ADD_UNCONDITIONAL))
  addSMRelRootMenu.add(mb.createMenuItem("Unless", this, ADD_UNLESS))
  addSMRelRootMenu.add(mb.createMenuItem("Volitional_Cause", this, ADD_VOLITIONAL_CAUSE))
  addSMRelRootMenu.add(mb.createMenuItem("Volitional_Result", this, ADD_VOLITIONAL_RESULT))
  val addMNRelRootMenu: JMenu = mb.createMenu("AddMNRel")
  addMNRelRootMenu.add(mb.createMenuItem("Conjunction", this, ADD_CONJUNCTION))
  addMNRelRootMenu.add(mb.createMenuItem("Contrast", this, ADD_CONTRAST))
  addMNRelRootMenu.add(mb.createMenuItem("Disjunction", this, ADD_DISJUNCTION))
  addMNRelRootMenu.add(mb.createMenuItem("Joint", this, ADD_JOINT))
  addMNRelRootMenu.add(mb.createMenuItem("List", this, ADD_LIST))
  addMNRelRootMenu.add(mb.createMenuItem("Multinuclear_Restatement", this, ADD_MULTINUCLEAR_RESTATEMENT))
  addMNRelRootMenu.add(mb.createMenuItem("Sequence", this, ADD_SEQUENCE))
  rootMenu.add(addRepRelRootMenu)
  rootMenu.add(addSMRelRootMenu)
  rootMenu.add(addMNRelRootMenu)
  rootMenu.addSeparator()
  rootMenu.add(mb.createMenuItem("ExpandAll", this, EXPAND_ALL))
  rootMenu.addSeparator()
  rootMenu.add(mb.createMenuItem("Delete", this, DELETE))

  val relationMenu = new JPopupMenu
  val addRepRelRelMenu: JMenu = mb.createMenu("AddRepRel")
  addRepRelRelMenu.add(mb.createMenuItem("Antithesis", this, ADD_ANTITHESIS))
  addRepRelRelMenu.add(mb.createMenuItem("Background", this, ADD_BACKGROUND))
  addRepRelRelMenu.add(mb.createMenuItem("Concession", this, ADD_CONCESSION))
  addRepRelRelMenu.add(mb.createMenuItem("Enablement", this, ADD_ENABLEMENT))
  addRepRelRelMenu.add(mb.createMenuItem("Evidence", this, ADD_EVIDENCE))
  addRepRelRelMenu.add(mb.createMenuItem("Justify", this, ADD_JUSTIFY))
  addRepRelRelMenu.add(mb.createMenuItem("Motivation", this, ADD_MOTIVATION))
  addRepRelRelMenu.add(mb.createMenuItem("Preparation", this, ADD_PREPARATION))
  addRepRelRelMenu.add(mb.createMenuItem("Restatement", this, ADD_RESTATEMENT))
  addRepRelRelMenu.add(mb.createMenuItem("Summary", this, ADD_SUMMARY))
  val addSMRelRelMenu: JMenu = mb.createMenu("AddSMRel")
  addSMRelRelMenu.add(mb.createMenuItem("Analysis", this, ADD_ANALYSIS))
  addSMRelRelMenu.add(mb.createMenuItem("Circumstance", this, ADD_CIRCUMSTANCE))
  addSMRelRelMenu.add(mb.createMenuItem("Condition", this, ADD_CONDITION))
  addSMRelRelMenu.add(mb.createMenuItem("Elaboration", this, ADD_ELABORATION))
  addSMRelRelMenu.add(mb.createMenuItem("Emphasis", this, ADD_EMPHASIS))
  addSMRelRelMenu.add(mb.createMenuItem("Evaluation", this, ADD_EVALUATION))
  addSMRelRelMenu.add(mb.createMenuItem("Interpretation", this, ADD_INTERPRETATION))
  addSMRelRelMenu.add(mb.createMenuItem("Means", this, ADD_MEANS))
  addSMRelRelMenu.add(mb.createMenuItem("Non-volitional_Cause", this, ADD_NON_VOLITIONAL_CAUSE))
  addSMRelRelMenu.add(mb.createMenuItem("Non-volitional_Result", this, ADD_NON_VOLITIONAL_RESULT))
  addSMRelRelMenu.add(mb.createMenuItem("Otherwise", this, ADD_OTHERWISE))
  addSMRelRelMenu.add(mb.createMenuItem("Purpose", this, ADD_PURPOSE))
  addSMRelRelMenu.add(mb.createMenuItem("Solutionhood", this, ADD_SOLUTIONHOOD))
  addSMRelRelMenu.add(mb.createMenuItem("Unconditional", this, ADD_UNCONDITIONAL))
  addSMRelRelMenu.add(mb.createMenuItem("Unless", this, ADD_UNLESS))
  addSMRelRelMenu.add(mb.createMenuItem("Volitional_Cause", this, ADD_VOLITIONAL_CAUSE))
  addSMRelRelMenu.add(mb.createMenuItem("Volitional_Result", this, ADD_VOLITIONAL_RESULT))
  val addMNRelRelMenu: JMenu = mb.createMenu("AddMNRel")
  addMNRelRelMenu.add(mb.createMenuItem("Conjunction", this, ADD_CONJUNCTION))
  addMNRelRelMenu.add(mb.createMenuItem("Contrast", this, ADD_CONTRAST))
  addMNRelRelMenu.add(mb.createMenuItem("Disjunction", this, ADD_DISJUNCTION))
  addMNRelRelMenu.add(mb.createMenuItem("Joint", this, ADD_JOINT))
  addMNRelRelMenu.add(mb.createMenuItem("List", this, ADD_LIST))
  addMNRelRelMenu.add(mb.createMenuItem("Multinuclear_Restatement", this, ADD_MULTINUCLEAR_RESTATEMENT))
  addMNRelRelMenu.add(mb.createMenuItem("Sequence", this, ADD_SEQUENCE))
  relationMenu.add(addRepRelRelMenu)
  relationMenu.add(addSMRelRelMenu)
  relationMenu.add(addMNRelRelMenu)
  relationMenu.addSeparator()
  val changeToRepRelMenu: JMenu = mb.createMenu("ChangeToRepRel")
  changeToRepRelMenu.add(mb.createMenuItem("Antithesis", this, CHANGE_TO_ANTITHESIS))
  changeToRepRelMenu.add(mb.createMenuItem("Background", this, CHANGE_TO_BACKGROUND))
  changeToRepRelMenu.add(mb.createMenuItem("Concession", this, CHANGE_TO_CONCESSION))
  changeToRepRelMenu.add(mb.createMenuItem("Enablement", this, CHANGE_TO_ENABLEMENT))
  changeToRepRelMenu.add(mb.createMenuItem("Evidence", this, CHANGE_TO_EVIDENCE))
  changeToRepRelMenu.add(mb.createMenuItem("Justify", this, CHANGE_TO_JUSTIFY))
  changeToRepRelMenu.add(mb.createMenuItem("Motivation", this, CHANGE_TO_MOTIVATION))
  changeToRepRelMenu.add(mb.createMenuItem("Preparation", this, CHANGE_TO_PREPARATION))
  changeToRepRelMenu.add(mb.createMenuItem("Restatement", this, CHANGE_TO_RESTATEMENT))
  changeToRepRelMenu.add(mb.createMenuItem("Summary", this, CHANGE_TO_SUMMARY))
  val changeToSMRelMenu: JMenu = mb.createMenu("ChangeToSMRel")
  changeToSMRelMenu.add(mb.createMenuItem("Analysis", this, CHANGE_TO_ANALYSIS))
  changeToSMRelMenu.add(mb.createMenuItem("Circumstance", this, CHANGE_TO_CIRCUMSTANCE))
  changeToSMRelMenu.add(mb.createMenuItem("Condition", this, CHANGE_TO_CONDITION))
  changeToSMRelMenu.add(mb.createMenuItem("Elaboration", this, CHANGE_TO_ELABORATION))
  changeToSMRelMenu.add(mb.createMenuItem("Emphasis", this, CHANGE_TO_EMPHASIS))
  changeToSMRelMenu.add(mb.createMenuItem("Evaluation", this, CHANGE_TO_EVALUATION))
  changeToSMRelMenu.add(mb.createMenuItem("Interpretation", this, CHANGE_TO_INTERPRETATION))
  changeToSMRelMenu.add(mb.createMenuItem("Means", this, CHANGE_TO_MEANS))
  changeToSMRelMenu.add(mb.createMenuItem("Non-volitional_Cause", this, CHANGE_TO_NON_VOLITIONAL_CAUSE))
  changeToSMRelMenu.add(mb.createMenuItem("Non-volitional_Result", this, CHANGE_TO_NON_VOLITIONAL_RESULT))
  changeToSMRelMenu.add(mb.createMenuItem("Otherwise", this, CHANGE_TO_OTHERWISE))
  changeToSMRelMenu.add(mb.createMenuItem("Purpose", this, CHANGE_TO_PURPOSE))
  changeToSMRelMenu.add(mb.createMenuItem("Solutionhood", this, CHANGE_TO_SOLUTIONHOOD))
  changeToSMRelMenu.add(mb.createMenuItem("Unconditional", this, CHANGE_TO_UNCONDITIONAL))
  changeToSMRelMenu.add(mb.createMenuItem("Unless", this, CHANGE_TO_UNLESS))
  changeToSMRelMenu.add(mb.createMenuItem("Volitional_Cause", this, CHANGE_TO_VOLITIONAL_CAUSE))
  changeToSMRelMenu.add(mb.createMenuItem("Volitional_Result", this, CHANGE_TO_VOLITIONAL_RESULT))
  val changeToMNRelMenu: JMenu = mb.createMenu("ChangeToMNRel")
  changeToMNRelMenu.add(mb.createMenuItem("Conjunction", this, CHANGE_TO_CONJUNCTION))
  changeToMNRelMenu.add(mb.createMenuItem("Contrast", this, CHANGE_TO_CONTRAST))
  changeToMNRelMenu.add(mb.createMenuItem("Disjunction", this, CHANGE_TO_DISJUNCTION))
  changeToMNRelMenu.add(mb.createMenuItem("Joint", this, CHANGE_TO_JOINT))
  changeToMNRelMenu.add(mb.createMenuItem("List", this, CHANGE_TO_LIST))
  changeToMNRelMenu.add(mb.createMenuItem("Multinuclear_Restatement", this, CHANGE_TO_MULTINUCLEAR_RESTATEMENT))
  changeToMNRelMenu.add(mb.createMenuItem("Sequence", this, CHANGE_TO_SEQUENCE))
  val changeRelationToNucleusItem: JMenuItem = mb.createMenuItem("ChangeToNucleus", this, CHANGE_TO_NUCLEUS)
  val changeRelationToSatelliteItem: JMenuItem = mb.createMenuItem("ChangeToSatellite", this, CHANGE_TO_SATELLITE)
  relationMenu.add(changeToRepRelMenu)
  relationMenu.add(changeToSMRelMenu)
  relationMenu.add(changeToMNRelMenu)
  relationMenu.addSeparator()
  relationMenu.add(changeRelationToNucleusItem)
  relationMenu.add(changeRelationToSatelliteItem)
  relationMenu.addSeparator()
  relationMenu.add(mb.createMenuItem("ExpandAll", this, EXPAND_ALL))
  relationMenu.addSeparator()
  relationMenu.add(mb.createMenuItem("Disband", this, DISBAND))
  relationMenu.addSeparator()
  relationMenu.add(mb.createMenuItem("Delete", this, DELETE))
  
  val normalChainMenu = new JPopupMenu
  val changeChainToNucleusItem: JMenuItem = mb.createMenuItem("ChangeToNucleus", this, CHANGE_TO_NUCLEUS)
  val changeChainToSatelliteItem: JMenuItem = mb.createMenuItem("ChangeToSatellite", this, CHANGE_TO_SATELLITE)
  normalChainMenu.add(changeChainToNucleusItem)
  normalChainMenu.add(changeChainToSatelliteItem)
  normalChainMenu.addSeparator()
  normalChainMenu.add(mb.createMenuItem("Delete", this, DELETE))
  
  val rawChainMenu = new JPopupMenu
  rawChainMenu.add(mb.createMenuItem("AddToRoot", this, ADD_TO_ROOT))
  rawChainMenu.addSeparator()
  rawChainMenu.add(mb.createMenuItem("Delete", this, DELETE))
  
  val rawMenu = new JPopupMenu
  rawMenu.add(mb.createMenuItem("Generate", this, GENERATE))
  rawMenu.add(mb.createMenuItem("Clear", this, CLEAR))
  
  val render = new RSTTreeCellRenderer
  setCellRenderer(render)
  
  addMouseListener(this)
  addKeyListener(this)
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.SYNCHED)
  ActionEventDispatcher.addActionListener(this, Command.TEXT_SPAN_DELETED)

  var model: RSTTreeModel = _
  
  var prevSelectedNode: RSTNode = _
  
  def display(fileName: String) {
    prevSelectedNode = null
    model = Resource.getTreeModel(fileName)
    if(Resource.autoGenerate && model.isRaw) {
      model.generate()
    }
    setModel(model)
    model.refresh()
    expandNode(model.getRoot.asInstanceOf[RSTRootNode])
  }
  
  def selectNode(node: RSTNode) {
    val path = new TreePath(model.getPathToRoot(node).asInstanceOf[Array[Object]])
    selectNode(path)
  }
  
  def selectNode(startOffset: Int) {
    val path = model.getTreePath(startOffset)
    selectNode(path)
  }
  
  def selectNode(path: TreePath) {
    if(path != null) {
      prevSelectedNode = path.getLastPathComponent.asInstanceOf[RSTNode]
      prevSelectedNode.highlight()
      setSelectionPath(path)
    }
  }
  
  def expandNode(target: RSTNode) {
    val nodes = new mutable.MutableList[LexicalChainInstanceNode]
    target match {
      case node1: RSTRootNode => node1.extractAllChainInstanceNodes(nodes)
      case node: RelationNode => node.extractAllChainInstanceNodes(nodes)
      case _ =>
    }
    for(node <- nodes) {
      val path = new TreePath(model.getPathToRoot(node).asInstanceOf[Array[Object]])
      setExpandedState(path, true)
    }
  }
  
  def actionPerformed(event: ActionEvent): Unit = {
    val selected = getSelectedNode
    event.getActionCommand match {
      case CHANGE_TO_NUCLEUS =>
        val parent = selected.getParent.asInstanceOf[RelationNode]
        parent.switchNS()
        selected.highlight()
        ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
      case CHANGE_TO_SATELLITE =>
        val parent = selected.getParent.asInstanceOf[RelationNode]
        parent.switchNS()
        selected.highlight()
        ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
      case DELETE =>
        selected.delete()
        prevSelectedNode = null
        ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
      case CLEAR =>
        selected.delete()
      case DISBAND =>
        selected.asInstanceOf[RelationNode].disband()
        prevSelectedNode = null
        val path = new TreePath(model.getRoot)
        selectNode(path)
        ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
      case EXPAND_ALL =>
        expandNode(selected)
      case GENERATE =>
        selected.asInstanceOf[RawNode].generate()
        selected.asInstanceOf[RawNode].refresh()
      case ADD_TO_ROOT =>
        val root = model.getRoot.asInstanceOf[RSTRootNode]
        root.rawNode.removeChild(selected)
        root.addChild(selected)
        selectNode(selected)
      case Command.RESOURCE_LOADED => setRowHeight(Resource.fontSize + 4)
      case Command.SYNCHED =>
        if(model != null) {
          val path = getSelectionPath
          model.refresh()
          selectNode(path)
        }
      case Command.TEXT_SPAN_DELETED =>
        if(model != null) {
          val span = event.getSource.asInstanceOf[TextSpan]
          model.adjustTree(span)
//          val path = getSelectionPath
//          println(s"adjust done")
//          model.refresh
//          println(s"refresh done")
//          selectNode(path)
        }
      case _ =>
        event.getActionCommand.substring(0, 1) match {
          case "a" =>
            val newNode = new RelationNode(event.getActionCommand.substring(2), selected.UNDEFINED_ROLE, model)
            selected.addChild(newNode)
          case "c" =>
            selected.asInstanceOf[RelationNode].setCategory(event.getActionCommand.substring(2))
            selected.highlight()
        }
        ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
    }
  }

  def mouseClicked(e: MouseEvent): Unit = {}

  def mouseEntered(e: MouseEvent): Unit = {}

  def mouseExited(e: MouseEvent): Unit = {}
  
  def getSelectedNode: RSTNode = {
    if(model != null) {
      val path = getSelectionPath
      if(path != null)
        path.getLastPathComponent.asInstanceOf[RSTNode]
      else
        model.getRoot.asInstanceOf[RSTNode]
    }
    else
      null
  }

  def mousePressed(e: MouseEvent): Unit = {
    val currSelectedNode = getSelectedNode
    if(currSelectedNode == null)
      return
    if(e.getButton == MouseEvent.BUTTON3) {
      currSelectedNode match {
        case _: RSTRootNode =>
          setComponentPopupMenu(rootMenu)
        case relNode: RelationNode =>
          if (relNode.getChildCount < relNode.CAPACITY) {
            addRepRelRelMenu.setEnabled(true)
            addSMRelRelMenu.setEnabled(true)
            addMNRelRelMenu.setEnabled(true)
          }
          else {
            addRepRelRelMenu.setEnabled(false)
            addSMRelRelMenu.setEnabled(false)
            addMNRelRelMenu.setEnabled(false)
          }
          if (currSelectedNode.role != currSelectedNode.UNDEFINED_ROLE) {
            val parent = currSelectedNode.getParent.asInstanceOf[RelationNode]
            if (parent.isNSRelation) {
              if (currSelectedNode.role == currSelectedNode.NUCLEUS_ROLE) {
                changeRelationToNucleusItem.setEnabled(false)
                changeRelationToSatelliteItem.setEnabled(true)
              }
              else {
                changeRelationToNucleusItem.setEnabled(true)
                changeRelationToSatelliteItem.setEnabled(false)
              }
            }
            else {
              changeRelationToNucleusItem.setEnabled(false)
              changeRelationToSatelliteItem.setEnabled(false)
            }
          }
          else {
            changeRelationToNucleusItem.setEnabled(false)
            changeRelationToSatelliteItem.setEnabled(false)
          }
          setComponentPopupMenu(relationMenu)
        case _: RawNode =>
          setComponentPopupMenu(rawMenu)
        case _ =>
          if (currSelectedNode.getParent.isInstanceOf[RawNode]) {
            setComponentPopupMenu(rawChainMenu)
          } else {
            if (currSelectedNode.role != currSelectedNode.UNDEFINED_ROLE) {
              val parent = currSelectedNode.getParent.asInstanceOf[RelationNode]
              if (parent.isNSRelation) {
                if (currSelectedNode.role == currSelectedNode.NUCLEUS_ROLE) {
                  changeChainToNucleusItem.setEnabled(false)
                  changeChainToSatelliteItem.setEnabled(true)
                }
                else {
                  changeChainToNucleusItem.setEnabled(true)
                  changeChainToSatelliteItem.setEnabled(false)
                }
              }
              else {
                changeChainToNucleusItem.setEnabled(false)
                changeChainToSatelliteItem.setEnabled(false)
              }
            }
            else {
              changeChainToNucleusItem.setEnabled(false)
              changeChainToSatelliteItem.setEnabled(false)
            }
            setComponentPopupMenu(normalChainMenu)
          }
      }
    }
    if(currSelectedNode != prevSelectedNode) {
      if(prevSelectedNode != null)
        prevSelectedNode.dehighlight()
      prevSelectedNode = currSelectedNode
      currSelectedNode.highlight()
      currSelectedNode match {
        case node: LexicalChainInstanceNode if !node.raw => ActionEventDispatcher.fireActionEvent(new ActionEvent(currSelectedNode, 0, Command.LEXICAL_CHAIN_INSTANCE_SELECTED))
        case _ =>
      }
    }
  }

  def mouseReleased(e: MouseEvent): Unit = {}
  
  override def keyReleased(e: KeyEvent): Unit = {
    e.getKeyCode match {
      case KeyEvent.VK_DELETE =>
        val paths = getSelectionPaths
        if(paths.size == 1) {
          val node = paths.apply(0).getLastPathComponent.asInstanceOf[RSTNode]
          if(node.isInstanceOf[RawNode]) {
            node.delete()
          }
          else {
            node.delete()
            prevSelectedNode = null
            ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
          }
        }
        else if(paths.size > 1) {
          var deleted = false
          for(path <- paths) {
            val node = path.getLastPathComponent.asInstanceOf[RSTNode]
            if(node.isInstanceOf[LexicalChainInstanceNode]) {
              deleted = true
              node.delete()
            }
          }
          if(deleted) {
            prevSelectedNode = null
            ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
          }
        }
      case _ =>
    }
  }
}

class RSTTreeCellRenderer extends DefaultTreeCellRenderer {
  
  val dummyIcon = new ImageIcon("icons/dummy.png")
  
  val rootIcon = new ImageIcon("icons/document.png")
  val emptyRootIcon = new ImageIcon("icons/document_empty.png")
  val warningRootIcon = new ImageIcon("icons/document_warning.png")
  
  val rawIcon = new ImageIcon("icons/raw.png")
  
  val nucleusTodoIcon = new ImageIcon("icons/todo_nucleus.png")
  val satelliteTodoIcon = new ImageIcon("icons/todo_satellite.png")
  val undefinedTodoIcon = new ImageIcon("icons/todo_undefined.png")
  
  val nucleusNSIcon = new ImageIcon("icons/ns_nucleus.png")
  val satelliteNSIcon = new ImageIcon("icons/ns_satellite.png")
  val undefinedNSIcon = new ImageIcon("icons/ns_undefined.png")
  
  val nucleusSNIcon = new ImageIcon("icons/sn_nucleus.png")
  val satelliteSNIcon = new ImageIcon("icons/sn_satellite.png")
  val undefinedSNIcon = new ImageIcon("icons/sn_undefined.png")
  
  val nucleusNNIcon = new ImageIcon("icons/nn_nucleus.png")
  val satelliteNNIcon = new ImageIcon("icons/nn_satellite.png")
  val undefinedNNIcon = new ImageIcon("icons/nn_undefined.png")
  
  val illegalChainIcon = new ImageIcon("icons/chain_warning.png")
  val nucleusChainIcon = new ImageIcon("icons/chain_nucleus.png")
  val satelliteChainIcon = new ImageIcon("icons/chain_satellite.png")
  val undefineChainIcon = new ImageIcon("icons/chain_undefined.png")
  
  override def getTreeCellRendererComponent(tree: JTree, value: Object, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean): Component = {
    super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    val path = tree.getPathForRow(row)
    if(path != null) {
      val icon = {
        val node = path.getLastPathComponent
        node match {
          case node1: RSTRootNode =>
            if (node1.isEmpty)
              emptyRootIcon
            else if (node1.isComplete && node1.isLegal)
              rootIcon
            else
              warningRootIcon
          case _: RawNode =>
            rawIcon
          case relNode: RelationNode =>
            if (relNode.getChildCount < relNode.CAPACITY) {
              relNode.role match {
                case relNode.NUCLEUS_ROLE => nucleusTodoIcon
                case relNode.SATELLITE_ROLE => satelliteTodoIcon
                case _ => undefinedTodoIcon
              }
            }
            else {
              if (relNode.isNSRelation) {
                if (relNode.isSN) {
                  relNode.role match {
                    case relNode.NUCLEUS_ROLE => nucleusSNIcon
                    case relNode.SATELLITE_ROLE => satelliteSNIcon
                    case _ => undefinedSNIcon
                  }
                }
                else {
                  relNode.role match {
                    case relNode.NUCLEUS_ROLE => nucleusNSIcon
                    case relNode.SATELLITE_ROLE => satelliteNSIcon
                    case _ => undefinedNSIcon
                  }
                }
              }
              else {
                relNode.role match {
                  case relNode.NUCLEUS_ROLE => nucleusNNIcon
                  case relNode.SATELLITE_ROLE => satelliteNNIcon
                  case _ => undefinedNNIcon
                }
              }
            }
          case chainNode: LexicalChainInstanceNode =>
            if (chainNode.isLegal) {
              chainNode.role match {
                case chainNode.NUCLEUS_ROLE => nucleusChainIcon
                case chainNode.SATELLITE_ROLE => satelliteChainIcon
                case _ => undefineChainIcon
              }
            }
            else
              illegalChainIcon
          case _ =>
            dummyIcon
        }
      }
      setIcon(icon)
      setFont(Resource.defaultFont)
    }
    this
  }
}