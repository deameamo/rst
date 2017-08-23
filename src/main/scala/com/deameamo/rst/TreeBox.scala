package com.deameamo.rst

import java.awt.{Color, Font}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{BorderFactory, JScrollPane}
import javax.swing.border.TitledBorder
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeModel}

import com.deameamo.event.ActionEventDispatcher
import com.deameamo.swingx.ResizableBox

class TreeBox extends ResizableBox(ResizableBox.VERTICAL) with ActionListener {

  val song12Font = new Font("宋体", Font.PLAIN, 12)
  
  var currFileName: String = _
  
  val tree = new RSTTree
  val dummyRoot = new DefaultMutableTreeNode("dummy")
  val dummyModel = new DefaultTreeModel(dummyRoot)
  val treeSPane = new JScrollPane(tree)
  new RSTTreeDragSource(tree)
  new RSTTreeDropTarget(tree)
  Resource.setTree(tree)
  val treeBox = new ResizableBox
  treeBox.addItem(treeSPane)
  
  val finder = new RSTRelationFinderBox
  
  addItem(treeBox, 0.6)
  addItem(finder, 0.4)
  
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.SAVE)
  ActionEventDispatcher.addActionListener(this, Command.FILE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.TREE_MODEL_CHANGED)
  ActionEventDispatcher.addActionListener(this, Command.TREE_MODEL_SYNCHED)
  initialize()
  
  var synched: Boolean = true
  def notifyDataChanged(): Unit = {
    synched = false
    treeBox.setBackground(Color.ORANGE)
    if(tree.model != null)
      tree.model.synched = synched
  }
  def notifyDataSynched(): Unit = {
    synched = true
    treeBox.setBackground(null)
    if(tree.model != null)
      tree.model.synched = synched
  }
  
  def initialize() {
    setTitle("dummy")
    tree.setModel(dummyModel)
    currFileName = null
    notifyDataSynched()
  }
  
  def setTitle(title: String) {
    treeBox.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createRaisedBevelBorder,
        title, TitledBorder.CENTER, TitledBorder.ABOVE_TOP, song12Font))
  }

  def actionPerformed(event: ActionEvent): Unit = {
    event.getActionCommand match {
      case Command.RESOURCE_LOADED =>
        initialize()
        tree.setBackground(Resource.getColor("bg"))
        tree.render.setBackgroundNonSelectionColor(Resource.getColor("bg"))
      case Command.FILE_LOADED =>
        val msg = event.getSource.asInstanceOf[FileLoadedMessage]
        if(msg.position != null) {
          if(currFileName == msg.position.fileName) {
            if(msg.position.startOffset != -1) {
              tree.model.refresh()
              tree.selectNode(msg.position.startOffset)
            }
          }
          else {
            setTitle(msg.position.fileName)
            tree.display(msg.position.fileName)
            currFileName = msg.position.fileName
            if(msg.position.startOffset != -1)
              tree.selectNode(msg.position.startOffset)
          }
        }
        else {
          if(currFileName == msg.relationNode.model.fileName) {
            tree.model.refresh()
            tree.selectNode(msg.relationNode)
          }
          else {
            setTitle(msg.relationNode.model.fileName)
            tree.display(msg.relationNode.model.fileName)
            currFileName = msg.relationNode.model.fileName
            tree.selectNode(msg.relationNode)
          }
        }
      case Command.TREE_MODEL_CHANGED => notifyDataChanged()
      case Command.TREE_MODEL_SYNCHED => notifyDataSynched()
      case Command.SAVE =>
        if(currFileName != null && !synched) {
          notifyDataSynched()
        }
      case _ =>
    }
  }
}