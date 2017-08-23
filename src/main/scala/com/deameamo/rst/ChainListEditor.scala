package com.deameamo.rst

import java.awt.{BorderLayout, Color, Font}
import java.awt.event.{ActionEvent, ActionListener, KeyEvent, KeyListener}
import javax.swing._
import javax.swing.border.TitledBorder

import com.deameamo.event.ActionEventDispatcher
import com.deameamo.swingx.{PopupMenuTextField, ResizableBox}

class ChainListEditor(title: String, listName: String)
    extends ResizableBox(ResizableBox.VERTICAL, false) with ActionListener {
  
  val song12Font = new Font("宋体", Font.PLAIN, 12)
  val editorFont = new Font("宋体", Font.ITALIC, 14)
  setBorder(BorderFactory.createTitledBorder(
      BorderFactory.createRaisedBevelBorder,
      title, TitledBorder.CENTER, TitledBorder.ABOVE_TOP, song12Font))
  
  var showingList = true
  
  val listPanel = new ResizableBox(ResizableBox.VERTICAL, false)
  
  val model = new ChainListModel(listName)
  val chainList = new JList(model)
  chainList.addKeyListener(new KeyListener {
    def keyReleased(event: KeyEvent): Unit = {
      event.getKeyCode match {
        case KeyEvent.VK_UP =>
          model.moveUp(chainList.getSelectedIndex)
          notifyDataChanged()
        case KeyEvent.VK_DOWN =>
          model.moveDown(chainList.getSelectedIndex)
          notifyDataChanged()
        case _ =>
      }}

    def keyPressed(event: KeyEvent): Unit = {
      event.getKeyCode match {
        case KeyEvent.VK_DELETE => listRemove()
        case _ =>
      }
    }
  
    def keyTyped(event: KeyEvent): Unit = {}
  })
  val listSPane = new JScrollPane(chainList)
  
  val addBox = new ResizableBox(ResizableBox.HORIZONTAL, false)
  val textField = new PopupMenuTextField
  textField.addKeyListener(new KeyListener {
    def keyPressed(event: KeyEvent): Unit = {}

    def keyReleased(event: KeyEvent): Unit = {
      event.getKeyCode match {
        case KeyEvent.VK_ENTER => listAdd()
        case _ =>
      }
    }
  
    def keyTyped(event: KeyEvent): Unit = {}
  })
  val icon = new ImageIcon("icons/add.png")
  val addButton = new JButton(icon)
  addButton.setActionCommand(Command.ADD)
  addButton.addActionListener(this)
  addBox.addItem(textField)
  addBox.addRigidItem(addButton, 24)
  
  listPanel.addItem(listSPane)
  listPanel.addRigidItem(addBox, 24)
  
  val editorPanel = new ResizableBox
  val textArea = new JTextArea
  textArea.addKeyListener(new KeyListener {
    def keyPressed(event: KeyEvent): Unit = {}
  
    def keyReleased(event: KeyEvent): Unit = {
      notifyDataChanged()
    }
  
    def keyTyped(event: KeyEvent): Unit = {}
  })
  val editorSPane = new JScrollPane(textArea)
  editorPanel.addItem(editorSPane)
  
  val switchButton = new JButton()
  val editorIcon = new ImageIcon("icons/editor.png")
  val listIcon = new ImageIcon("icons/list.png")
  switchButton.setText("Switch ")
  switchButton.setHorizontalTextPosition(SwingConstants.LEFT)
  setSwitchButtonIcon()
  switchButton.setActionCommand(Command.SWITCH)
  switchButton.addActionListener(this)
  
  val wrapperPanel = new JPanel
  wrapperPanel.setLayout(new BorderLayout)
  wrapperPanel.add(listPanel)
  
  addRigidItem(switchButton, 25)
  addItem(wrapperPanel)
  
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.SAVE)
  ActionEventDispatcher.addActionListener(this, Command.EDITOR_ADD_CHAIN)
//  ActionDispatcher.registerActionHandler(this, Action.EDITOR_REMOVE_CHAIN)
  
  
  var synched = true
  def notifyDataChanged(): Unit = {
    synched = false
    setBackground(Color.ORANGE)
  }
  def notifyDataSynched(): Unit = {
    synched = true
    setBackground(null)
  }
  
  def setSwitchButtonIcon() {
    if(showingList) {
      switchButton.setIcon(listIcon)
    }
    else {
      switchButton.setIcon(editorIcon)
    }
  }
  
  def removeChain(i: Int) {
    if(i != -1){
      model.removeChain(i)
      notifyDataChanged()
    }
  }
  
  def save() {
    if(!showingList) {
      Resource.replaceChainList(listName, textArea.getText)
      if(!synched)
        ActionEventDispatcher.fireActionEvent(Command.SYNCH)
    }
    Resource.saveChainList(listName)
    notifyDataSynched()
  }
  
  def switch() {
    if(showingList) {
      textArea.setText(Resource.getChainList(listName).mkString("\n"))
      textArea.setCaretPosition(0)
      wrapperPanel.remove(listPanel)
      wrapperPanel.add(editorPanel)
    }
    else {
      Resource.replaceChainList(listName, textArea.getText)
      model.synch()
      wrapperPanel.remove(editorPanel)
      wrapperPanel.add(listPanel)
      if(!synched)
        ActionEventDispatcher.fireActionEvent(Command.SYNCH)
    }
    showingList = !showingList
    setSwitchButtonIcon()
    updateUI()
  }
  
  def addChain(string: String){
    if(model.addChain(string)) {
      notifyDataChanged()
      textField.setText("")
    }
  }
  
  def listAdd() {
    val string = textField.getText.trim
    if(string.length > 0 && !Resource.contains(listName, string)) {
      addChain(string)
      ActionEventDispatcher.fireActionEvent(Command.SYNCH)
    }
  }
  
  def listRemove() {
    val i = chainList.getSelectedIndex
    if(i != -1) {
      removeChain(i)
      if(i == chainList.getModel.getSize)
        chainList.setSelectedIndex(i - 1)
      else
        chainList.setSelectedIndex(i)
      ActionEventDispatcher.fireActionEvent(Command.SYNCH)
    }
  }

  def actionPerformed(event: ActionEvent): Unit = {
    event.getActionCommand match {
      case Command.ADD => listAdd()
      case Command.SWITCH => switch()
      case Command.RESOURCE_LOADED =>
        model.load()
        notifyDataSynched()
        chainList.setFont(Resource.defaultFont)
        textArea.setFont(Resource.defaultItalicFont)
        textArea.setBackground(Resource.getColor("bg"))
        textField.setBackground(Resource.getColor("bg"))
        chainList.setBackground(Resource.getColor("bg"))
      case Command.SAVE => save()
      case Command.EDITOR_ADD_CHAIN =>
        val addEvent = event.getSource.asInstanceOf[ChangeMessage]
        if(addEvent.listName == listName)
          addChain(addEvent.word)
      //      case Action.EDITOR_REMOVE_CHAIN => {
//        val removeEvent = event.getSource.asInstanceOf[ChangeMessage]
//        if(removeEvent.listName == listName)
//          removeChain(removeEvent.word)
//      }
      case _ => 
    }
  }
}