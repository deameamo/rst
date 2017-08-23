package com.deameamo.rst

import java.awt.{Color, Font}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.border.TitledBorder
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.{BorderFactory, JCheckBox, JScrollPane, border}

import com.deameamo.swingx.ResizableBox
import com.deameamo.event.ActionEventDispatcher

import scala.collection.mutable

class LexicalChainPanel extends ResizableBox(ResizableBox.VERTICAL) with ActionListener {

  val song12Font = new Font("宋体", Font.PLAIN, 12)
  val etchedBorder: border.Border = BorderFactory.createEtchedBorder
  
  val patternTree = new LexicalChainPatternTree
  val patternTreeSPane = new JScrollPane(patternTree)
  val autoCheckBox = new JCheckBox("Auto Assign Lexical Chain", false)
  autoCheckBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      ActionEventDispatcher.fireActionEvent(new ActionEvent(autoCheckBox.isSelected, 0, Command.AUTO_CHECK_CHANGED))
    }
  })
  
  new PatternTreeDragSource(patternTree)
  new PatternTreeDropTarget(patternTree)
  
  val patternTreeBox = new ResizableBox(ResizableBox.VERTICAL, false)
  patternTreeBox.addItem(patternTreeSPane)
  patternTreeBox.addRigidItem(autoCheckBox, 25)
  setTreeBoxTitle("All")
  
  val chainViewerBox = new LexicalChainViewerBox(patternTree.model)
  
  addItem(patternTreeBox, 0.6)
  addItem(chainViewerBox, 0.4)
  
  ActionEventDispatcher.addActionListener(this, Command.SAVE)
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.SHOW_CONTRADICTS)
  ActionEventDispatcher.addActionListener(this, Command.PATTERN_TREE_DATA_CHANGED)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_INSTANCE_VIEWED)
  
  def setTreeBoxTitle(title: String) {
    patternTreeBox.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createRaisedBevelBorder,
        title, TitledBorder.CENTER, TitledBorder.ABOVE_TOP, song12Font))
  }
  
  var synched = true
  def notifyDataSynched() {
    synched = true
    if(legalized)
      patternTreeBox.setBackground(null)
  }
  def notifyDataChanged() {
    synched = false
    if(legalized)
      patternTreeBox.setBackground(Color.ORANGE)
  }
  def notifySynchState() {
    if(synched)
      patternTreeBox.setBackground(null)
    else
      patternTreeBox.setBackground(Color.ORANGE)
  }
  
  var legalized = true
  def notifyDataContradicted() {
    legalized = false
    patternTreeBox.setBackground(Color.RED)
  }
  def notifyDataLegalized() {
    legalized = true
    notifySynchState()
  }
  
  def actionPerformed(event: ActionEvent) {
    event.getActionCommand match {
      case Command.RESOURCE_LOADED =>
        patternTree.model.load()
        //        notifyDataLegalized
        notifyDataSynched()
        patternTree.setBackground(Resource.getColor("bg"))
        patternTree.renderer.setBackgroundNonSelectionColor(Resource.getColor("bg"))
      case Command.SAVE =>
        Resource.savePatternTreeModel(patternTree.model)
        notifyDataSynched()
      case Command.SHOW_CONTRADICTS =>
        val contradicts = event.getSource.asInstanceOf[mutable.MutableList[DefaultMutableTreeNode]]
        if(contradicts.isEmpty)
          notifyDataLegalized()
        else
          notifyDataContradicted()
      case Command.PATTERN_TREE_DATA_CHANGED =>
        notifyDataChanged()
      case Command.LEXICAL_CHAIN_INSTANCE_VIEWED =>
        setTreeBoxTitle(event.getSource.asInstanceOf[LexicalChainPosition].node.getPatternName)
    }
  }
}