package com.deameamo.rst

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Font}
import javax.swing._
import javax.swing.border.TitledBorder
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import com.deameamo.event.ActionEventDispatcher
import com.deameamo.swingx.ResizableBox

import scala.collection.mutable

class EditorPanel extends ResizableBox(ResizableBox.HORIZONTAL, true) with ActionListener with ListSelectionListener {

  val song12Font = new Font("宋体", Font.PLAIN, 12)
  val etchedBorder: border.Border = BorderFactory.createEtchedBorder
//  setBorder(BorderFactory.createTitledBorder(
//      BorderFactory.createRaisedBevelBorder,
//      "文章", TitledBorder.CENTER, TitledBorder.ABOVE_TOP, song16Font))
  
//  setBorder(new RoundBorder(16))
  val editorPanel = new ResizableBox
  val editor = new RSTTextPane
  Resource.editor = editor
  val editorSPane = new JScrollPane(editor)
  editor.setCaretPosition(0)
  editorPanel.addItem(editorSPane)
  
//  val editorBlankBox = new ResizableBox
  val editorControlBox = new ResizableBox(ResizableBox.HORIZONTAL)
  val undoButton = new JButton("Undo")
  undoButton.setActionCommand(Command.UNDO)
  undoButton.addActionListener(this)
  val doneButton = new JButton("Done")
  doneButton.setActionCommand(Command.DONE)
  doneButton.addActionListener(this)
  editorControlBox.addItem(undoButton, 0.5)
  editorControlBox.addItem(doneButton, 0.5)
  
  val editorWrapperBox = new ResizableBox(ResizableBox.VERTICAL, false)
  editorWrapperBox.addItem(editorPanel)
  editorWrapperBox.setBorderLength(10)
  editorWrapperBox.addRigidItem(editorControlBox, 25)
  
  val todoListPanel = new FileListPanel("Todo list", Resource.TODO_LIST)
  todoListPanel.fileList.addListSelectionListener(editor)
  todoListPanel.fileList.addListSelectionListener(this)
  
  val doneListPanel = new FileListPanel("Done list", Resource.DONE_LIST)
  doneListPanel.fileList.addListSelectionListener(editor)
  doneListPanel.fileList.addListSelectionListener(this)
  
  undoButton.setEnabled(false)
  doneButton.setEnabled(false)
  
  var synched = true
  def notifyDataChanged(): Unit = {
    synched = false
    editorPanel.setBackground(Color.ORANGE)
  }
  def notifyDataSynched(): Unit = {
    synched = true
    editorPanel.setBackground(null)
  }
  
  ActionEventDispatcher.addActionListener(this, Command.LIST_EMPTIED)
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_INSTANCE_VIEWED)
  ActionEventDispatcher.addActionListener(this, Command.TEXT_SPAN_DELETED)
  ActionEventDispatcher.addActionListener(this, Command.SAVE)
  ActionEventDispatcher.addActionListener(this, Command.RELATION_NODE_VIEWED)
  
  var selectedList: JList[String] = _
  
  def valueChanged(event: ListSelectionEvent): Unit = {
    if(!event.getValueIsAdjusting) {
      val fileName = event.getSource.asInstanceOf[JList[String]].getSelectedValue
      if(fileName != null) {
        setEditorPanelTitle(fileName)
        if (event.getSource == todoListPanel.fileList) {
          undoButton.setEnabled(false)
          doneButton.setEnabled(true)
          if(doneListPanel.fileList.getSelectedIndex != -1) {
            doneListPanel.fileList.removeListSelectionListener(this)
            doneListPanel.fileList.clearSelection()
            doneListPanel.fileList.addListSelectionListener(this)
          }
        }
        else {
          undoButton.setEnabled(true)
          doneButton.setEnabled(false)
          if(todoListPanel.fileList.getSelectedIndex != -1) {
            todoListPanel.fileList.removeListSelectionListener(this)
            todoListPanel.fileList.clearSelection()
            todoListPanel.fileList.addListSelectionListener(this)
          }
        }
      }
    }
  }
  
  val listBox = new ResizableBox
  listBox.setResizable(false)
  listBox.addItem(todoListPanel, 0.5)
  listBox.addItem(doneListPanel, 0.5)
  addItem(listBox, 0.3)
  addItem(editorWrapperBox, 0.7)
  
  def setEditorPanelTitle(title: String) {
    editorPanel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createRaisedBevelBorder,
        title, TitledBorder.CENTER, TitledBorder.ABOVE_TOP, song12Font))
  }

  def actionPerformed(event: ActionEvent): Unit = {
    event.getActionCommand match {
      case Command.UNDO => move(doneListPanel, todoListPanel)
      case Command.DONE => move(todoListPanel, doneListPanel)
      case Command.LIST_EMPTIED =>
        val target = event.getSource.asInstanceOf[FileListPanel]
        target.fileList.setSelectedIndex(0)
        if(target == todoListPanel) {
          undoButton.setEnabled(false)
          doneButton.setEnabled(true)
        }
        else {
          undoButton.setEnabled(true)
          doneButton.setEnabled(false)
        }
        setEditorPanelTitle(target.model.list.get(0).get)
      case Command.RESOURCE_LOADED =>
        todoListPanel.model.load()
        doneListPanel.model.load()
        todoListPanel.fileList.setFont(Resource.defaultFont)
        doneListPanel.fileList.setFont(Resource.defaultFont)
        setEditorPanelTitle(" ")
        editor.setText(null)
        updateTitles()
        todoListPanel.fileList.setBackground(Resource.getColor("bg"))
        todoListPanel.renderer.setBackground(Resource.getColor("bg"))
        doneListPanel.fileList.setBackground(Resource.getColor("bg"))
        doneListPanel.renderer.setBackground(Resource.getColor("bg"))
      case Command.LEXICAL_CHAIN_INSTANCE_VIEWED =>
        todoListPanel.fileList.removeListSelectionListener(this)
        doneListPanel.fileList.removeListSelectionListener(this)
        todoListPanel.fileList.removeListSelectionListener(editor)
        doneListPanel.fileList.removeListSelectionListener(editor)
        val fileName = event.getSource.asInstanceOf[LexicalChainPosition].fileName
        setEditorPanelTitle(fileName)
        if(todoListPanel.model.list.contains(fileName)) {
          doneListPanel.fileList.clearSelection()
          todoListPanel.fileList.setSelectedIndex(todoListPanel.model.list.indexOf(fileName))
          undoButton.setEnabled(false)
          doneButton.setEnabled(true)
        }
        else {
          todoListPanel.fileList.clearSelection()
          doneListPanel.fileList.setSelectedIndex(doneListPanel.model.list.indexOf(fileName))
          undoButton.setEnabled(true)
          doneButton.setEnabled(false)
        }
        todoListPanel.fileList.addListSelectionListener(this)
        doneListPanel.fileList.addListSelectionListener(this)
        todoListPanel.fileList.addListSelectionListener(editor)
        doneListPanel.fileList.addListSelectionListener(editor)
      case Command.RELATION_NODE_VIEWED =>
        todoListPanel.fileList.removeListSelectionListener(this)
        doneListPanel.fileList.removeListSelectionListener(this)
        todoListPanel.fileList.removeListSelectionListener(editor)
        doneListPanel.fileList.removeListSelectionListener(editor)
        val fileName = event.getSource.asInstanceOf[RelationNode].model.fileName
        setEditorPanelTitle(fileName)
        if(todoListPanel.model.list.contains(fileName)) {
          doneListPanel.fileList.clearSelection()
          todoListPanel.fileList.setSelectedIndex(todoListPanel.model.list.indexOf(fileName))
          undoButton.setEnabled(false)
          doneButton.setEnabled(true)
        }
        else {
          todoListPanel.fileList.clearSelection()
          doneListPanel.fileList.setSelectedIndex(doneListPanel.model.list.indexOf(fileName))
          undoButton.setEnabled(true)
          doneButton.setEnabled(false)
        }
        todoListPanel.fileList.addListSelectionListener(this)
        doneListPanel.fileList.addListSelectionListener(this)
        todoListPanel.fileList.addListSelectionListener(editor)
        doneListPanel.fileList.addListSelectionListener(editor)
      case Command.TEXT_SPAN_DELETED =>
        notifyDataChanged()
      case Command.SAVE =>
        notifyDataSynched()
      case _ =>
    }
  }
  
  def move(from: FileListPanel, to: FileListPanel) {
    todoListPanel.fileList.removeListSelectionListener(this)
    doneListPanel.fileList.removeListSelectionListener(this)
    val selected = from.fileList.getSelectedValue
    if(selected != null) {
      val next = from.removeSelected()
      to.model.addFile(selected)
      if(next == null) {
        ActionEventDispatcher.fireActionEvent(new ActionEvent(to, 0, Command.LIST_EMPTIED))
      }
      else
        setEditorPanelTitle(next)
    }
    todoListPanel.fileList.addListSelectionListener(this)
    doneListPanel.fileList.addListSelectionListener(this)
    updateTitles()
  }
  
  def updateTitles() {
    todoListPanel.updateTitle()
    doneListPanel.updateTitle()
  }
}

class FileListPanel(title: String, listName: String) extends ResizableBox {
  
  val song12Font = new Font("宋体", Font.PLAIN, 12)
  val etchedBorder: border.Border = BorderFactory.createEtchedBorder
  
  val model = new FileListModel(listName)
  val fileList = new JList(model)
  val renderer = new FileListRenderer
  fileList.setCellRenderer(renderer)
  val listSPane = new JScrollPane(fileList)
  
  addItem(listSPane)
  
  def removeSelected(): String = {
    val i = fileList.getSelectedIndex
    model.removeFile(i)
    if(i < model.getSize) {
      fileList.setSelectedIndex(i)
      model.list.get(i).get
    }
    else if(i == model.getSize && model.getSize > 0) {
      fileList.setSelectedIndex(i - 1)
      model.list.get(i - 1).get
    }
    else
      null
  }
  
  def updateTitle() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createRaisedBevelBorder,
        s"$title(${model.getSize})", TitledBorder.CENTER, TitledBorder.ABOVE_TOP, song12Font))
  }
}

class FileListModel[E <: String](listName: String) extends DefaultListModel[String] {
  
  var list: mutable.MutableList[String] = _
  
  def load() {
    removeAllElements()
    list = Resource.getFileList(listName)
    for(name <- list) {
      add(getSize, name)
    }
  }
  
  def addFile(fileName: String) {
    list = Resource.addFile(listName, fileName)
    removeAllElements()
    for(name <- list) {
      add(getSize, name)
    }
  }
  
  def removeFile(i: Int) {
    list = Resource.removeFile(listName, i)
    remove(i)
  }
}