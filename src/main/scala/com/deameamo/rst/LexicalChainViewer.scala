package com.deameamo.rst

import java.awt.event.{ActionEvent, ActionListener, KeyEvent, KeyListener}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import com.deameamo.event.{ActionEventDispatcher, KeyAdapter}
import com.deameamo.swingx.{PopupMenuTextField, ResizableBox}

import scala.collection.mutable

class LexicalChainViewerBox(treeModel: LexicalChainPatternTreeModel) extends ResizableBox(ResizableBox.VERTICAL) with ActionListener {
  
  setPaddings(2, 2, 0, 0)
  val viewer = new LexicalChainViewer
  val viewerSPane = new JScrollPane(viewer)
  
  val searchBox = new ResizableBox(ResizableBox.HORIZONTAL, false)
  val textField = new PopupMenuTextField
  textField.addKeyListener(new KeyListener {
    def keyPressed(event: KeyEvent): Unit = {}

    def keyReleased(event: KeyEvent): Unit = {
      event.getKeyCode match {
        case KeyEvent.VK_ENTER => search()
        case _ =>
      }
    }
  
    def keyTyped(event: KeyEvent): Unit = {}
  })
  
  val icon = new ImageIcon("icons/search.png")
  val searchButton = new JButton(icon)
  searchButton.setActionCommand(Command.SEARCH_POSITIONS)
  searchButton.addActionListener(this)
  searchBox.addItem(textField)
  searchBox.addRigidItem(searchButton, 24)
  
  addItem(viewerSPane)
  addRigidItem(searchBox, 24)
  setBorder(BorderFactory.createRaisedBevelBorder)
  
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  
  def search() { viewer.display(treeModel.search(textField.getText.split(" ")), -1) }

  def actionPerformed(event: ActionEvent): Unit = {
    event.getActionCommand match {
      case Command.RESOURCE_LOADED =>
        textField.setBackground(Resource.getColor("bg"))
      case Command.SEARCH_POSITIONS => search()
    }
  }
}

class LexicalChainViewer[E <: String] extends JList[String] with ListSelectionListener with ActionListener with KeyAdapter {
  
  val model = new LexicalChainViewerModel
  setModel(model)
  setCellRenderer(new LexicalChainViewerRenderer)
  
  addKeyListener(this)
  addListSelectionListener(this)
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_SELECTED)
  ActionEventDispatcher.addActionListener(this, Command.SHOW_CONTRADICTS)
  
  override def keyReleased(e: KeyEvent) {
    e.getKeyCode match {
      case KeyEvent.VK_DELETE =>
        ActionEventDispatcher.fireActionEvent(new ActionEvent(model.currPositions.apply(getSelectedIndex), 0, Command.DELETE_POSITION))
      case KeyEvent.VK_UP =>
        fireAction()
      case KeyEvent.VK_DOWN =>
        fireAction()
      case _ =>
    }
  }
  
  def display(positions: mutable.MutableList[LexicalChainPosition], index: Int) {
    model.display(positions)
    if(index != -1) {
      setSelectedIndex(index)
      fireAction()
    }
  }
  
  def valueChanged(event: ListSelectionEvent) {
    if(event.getValueIsAdjusting) {
      fireAction()
    }
  }
  
  def fireAction() {
    val selected = model.currPositions.get(getSelectedIndex).get
    ActionEventDispatcher.fireActionEvent(new ActionEvent(selected, 0, Command.LEXICAL_CHAIN_INSTANCE_VIEWED))
  }

  def actionPerformed(event: ActionEvent) {
    event.getActionCommand match {
      case Command.RESOURCE_LOADED =>
        display(null, -1)
        setFont(Resource.defaultFont)
        setBackground(Resource.getColor("bg"))
      case Command.LEXICAL_CHAIN_SELECTED => display(event.getSource.asInstanceOf[LexicalChainNode].positions, event.getID)
      case Command.SHOW_CONTRADICTS => display(model.currPositions, -1)
    }
  }
}

class LexicalChainViewerModel[E <: String] extends DefaultListModel[String] {
  
  var currPositions: mutable.MutableList[LexicalChainPosition] = new mutable.MutableList[LexicalChainPosition]
  
  def display(positions: mutable.MutableList[LexicalChainPosition]) {
    removeAllElements()
    currPositions.clear
    if(positions != null) {
      currPositions ++= positions
      for(pos <- currPositions) {
        addElement(pos.toString)
      }
    }
  }
}

