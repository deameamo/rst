package com.deameamo.rst

import java.awt.event.{ActionEvent, ActionListener, KeyEvent}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import com.deameamo.event.{ActionEventDispatcher, KeyAdapter}
import com.deameamo.swingx.{PopupMenuTextField, ResizableBox}

import scala.collection.mutable

class RSTRelationFinderBox extends ResizableBox(ResizableBox.VERTICAL) with ActionListener {

  setPaddings(2, 2, 0, 0)
  val finder = new RSTRelationFinder
  val finderSPane = new JScrollPane(finder)
  
  val searchBox = new ResizableBox(ResizableBox.HORIZONTAL, false)
  val textField = new PopupMenuTextField
  textField.addKeyListener(new KeyAdapter {
    override def keyReleased(event: KeyEvent): Unit = {
      event.getKeyCode match {
        case KeyEvent.VK_ENTER => search()
        case _ =>
      }
    }
  })
  
  val icon = new ImageIcon("icons/search.png")
  val searchButton = new JButton(icon)
  searchButton.setActionCommand(Command.SEARCH_RELATIONS)
  searchButton.addActionListener(this)
  searchBox.addItem(textField)
  searchBox.addRigidItem(searchButton, 24)
  
  addItem(finderSPane)
  addRigidItem(searchBox, 24)
  setBorder(BorderFactory.createRaisedBevelBorder)
  
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  
  def search() {
    finder.display(Resource.searchRelationNode(textField.getText))
  }
  
  def actionPerformed(event: ActionEvent): Unit = {
    event.getActionCommand match {
      case Command.RESOURCE_LOADED =>
        textField.setBackground(Resource.getColor("bg"))
      case Command.SEARCH_RELATIONS => search()
    }
  }
}

class RSTRelationFinder[E <: String] extends JList[String] with ListSelectionListener with ActionListener with KeyAdapter {
  val model = new RSTRelationFinderModel
  setModel(model)
  
  addKeyListener(this)
  addListSelectionListener(this)
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  
  override def keyReleased(e: KeyEvent) {
    e.getKeyCode match {
      case KeyEvent.VK_UP =>
        fireAction()
      case KeyEvent.VK_DOWN =>
        fireAction()
      case _ =>
    }
  }
  
  def display(relations: mutable.MutableList[RelationNode]) {
    model.display(relations)
  }
  
  def fireAction(): Unit = ActionEventDispatcher.fireActionEvent(new ActionEvent(model.relations.apply(getSelectedIndex), 0, Command.RELATION_NODE_VIEWED))
  
  def valueChanged(event: ListSelectionEvent) {
    if(event.getValueIsAdjusting) {
      fireAction()
    }
  }
  
  def actionPerformed(event: ActionEvent) {
    event.getActionCommand match {
      case Command.RESOURCE_LOADED =>
        setFont(Resource.defaultFont)
        setBackground(Resource.getColor("bg"))
    }
  }
}

class RSTRelationFinderModel[E <: String] extends DefaultListModel[String] {
  var relations = new mutable.MutableList[RelationNode]
  
  def display(relationNodes: mutable.MutableList[RelationNode]) {
    removeAllElements()
    relations.clear
    if(relationNodes != null) {
      relations ++= relationNodes
      for(rel <- relations) {
        addElement(rel.getInfoString)
      }
    }
  }
}