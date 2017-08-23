package com.deameamo.rst

import java.awt.event._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.{JList, JMenuItem, JPopupMenu, JTextPane}

import com.deameamo.event.ActionEventDispatcher
import com.deameamo.swingx.MenuBuilder

import scala.collection.mutable

class RSTTextPane extends JTextPane
    with ListSelectionListener with ActionListener with MouseListener with KeyListener {
  
  val WAITING_FOR_B = "B"
  val EXPECTING_I = "I"
  
  var status: String = WAITING_FOR_B
  val chainBuffer = new mutable.MutableList[String]
  
  setEditable(false)
  
  var document: RSTDocument = _
  var fileName: String = _
  
  val TAG_CHAIN_BEGINNING = "TAG_CHAIN_HEAD"
  val TAG_CHAIN_INTERMEDIATE = "TAG_CHAIN_INTERMEDIATE"
  val TAG_CHAIN_ENDING = "TAG_CHAIN_ENDING"
  val ABORT = "ABORT"
  val TAG_CONN = "TAG_CONN"
  val DELETE_WORD = "DELETE_WORD"
  val ADD_LEXICAL_CHAIN_INSTANCE = "ADD_LEXICAL_CHAIN"
  val DELETE_SPAN = "DELETE_SPAN"
  
  val mb = new MenuBuilder("com.deameamo.rst.EditorPopupMenu")
  val mainMenu = new JPopupMenu
  mainMenu.add(mb.createMenuItem("TagChainBeginning", this, TAG_CHAIN_BEGINNING, 'B'))
  mainMenu.add(mb.createMenuItem("TagConn", this, TAG_CONN, 'C'))
  val deleteWordItem: JMenuItem = mb.createMenuItem("DeleteWord", this, DELETE_WORD, 'D')
  mainMenu.addSeparator()
  mainMenu.add(deleteWordItem)
  mainMenu.addSeparator()
  val addLexicalChainItem: JMenuItem = mb.createMenuItem("AddLexicalChain", this, ADD_LEXICAL_CHAIN_INSTANCE, 'L')
  mainMenu.add(addLexicalChainItem)
  mainMenu.addSeparator()
  val deleteSpanItem: JMenuItem = mb.createMenuItem("DeleteSpan", this, DELETE_SPAN)
  mainMenu.add(deleteSpanItem)
  
  val chainMenu = new JPopupMenu
  chainMenu.add(mb.createMenuItem("TagChainIntermediate", this, TAG_CHAIN_INTERMEDIATE, 'I'))
  chainMenu.add(mb.createMenuItem("TagChainEnding", this, TAG_CHAIN_ENDING, 'E'))
  chainMenu.addSeparator()
  chainMenu.add(mb.createMenuItem("Abort", this, ABORT, 'A'))
  
  addMouseListener(this)
//  addKeyListener(this)
  ActionEventDispatcher.addActionListener(this, Command.RESOURCE_LOADED)
//  ActionDispatcher.registerActionHandler(this, Action.SYNCH)
  ActionEventDispatcher.addActionListener(this, Command.LEXICAL_CHAIN_INSTANCE_VIEWED)
  ActionEventDispatcher.addActionListener(this, Command.RELATION_NODE_VIEWED)

  def display(fileName: String, startOffset: Int) {
    this.fileName = fileName
    if(document != null)
      document.displaying = false
    document = Resource.getRSTDocument(fileName)
    document.tag()
    document.displaying = true
    setDocument(document)
    if(startOffset == -1)
      setCaretPosition(0)
    else
      setCaretPosition(startOffset)
  }
  
  def addWord(listName: String, word: String) {
    ActionEventDispatcher.fireActionEvent(new ActionEvent(ChangeMessage(listName, word), 0, Command.EDITOR_ADD_WORD))
    ActionEventDispatcher.fireActionEvent(Command.SYNCH)
  }
  
  def addChain(listName: String, chainString: String) {
    ActionEventDispatcher.fireActionEvent(new ActionEvent(ChangeMessage(listName, chainString), 0, Command.EDITOR_ADD_CHAIN))
    ActionEventDispatcher.fireActionEvent(Command.SYNCH)
  }
  
  def removeWord(listName: String, word: String) {
    ActionEventDispatcher.fireActionEvent(new ActionEvent(ChangeMessage(listName, word), 0, Command.EDITOR_REMOVE_WORD))
    ActionEventDispatcher.fireActionEvent(Command.SYNCH)
  }
  
  def refreshText() {
    if(getText.length > 0) {
      val caret = getCaretPosition
      document.tag()
      val tree = Resource.getTree
      tree.getModel.asInstanceOf[RSTTreeModel].refresh()
      setCaretPosition(caret)
    }
  }
  
  def valueChanged(event: ListSelectionEvent): Unit = {
    if(!event.getValueIsAdjusting) {
      val fileName = event.getSource.asInstanceOf[JList[String]].getSelectedValue
      if(fileName != null) {
        display(fileName, -1)
        ActionEventDispatcher.fireActionEvent(new ActionEvent(FileLoadedMessage(LexicalChainPosition(null, fileName, -1, -1), null), 0, Command.FILE_LOADED))
      }
    }
  }

  def actionPerformed(event: ActionEvent) {
    event.getActionCommand match {
      case TAG_CONN => tagConn()
      case TAG_CHAIN_BEGINNING => tagChainBeginning()
      case TAG_CHAIN_INTERMEDIATE => tagChainIntermediate()
      case TAG_CHAIN_ENDING => tagChainEnding()
      case ABORT => abort()
      case DELETE_WORD => deleteWord()
      case DELETE_SPAN => deleteSpan()
      case ADD_LEXICAL_CHAIN_INSTANCE => addLexicalChainInstance()
      case Command.RESOURCE_LOADED =>
        document = new RSTDocument
        setDocument(document)
        setBackground(Resource.getColor("bg"))
      case Command.LEXICAL_CHAIN_INSTANCE_VIEWED =>
        val msg = event.getSource.asInstanceOf[LexicalChainPosition]
        if(msg.fileName != fileName)
          display(msg.fileName, msg.startOffset)
        else
          setCaretPosition(msg.startOffset)
        ActionEventDispatcher.fireActionEvent(new ActionEvent(FileLoadedMessage(event.getSource.asInstanceOf[LexicalChainPosition], null), 0, Command.FILE_LOADED))
      case Command.RELATION_NODE_VIEWED =>
        val relationNode = event.getSource.asInstanceOf[RelationNode]
        if(relationNode.model.fileName != fileName)
          display(relationNode.model.fileName, relationNode.startOffset)
        else
          setCaretPosition(relationNode.startOffset)
        ActionEventDispatcher.fireActionEvent(new ActionEvent(FileLoadedMessage(null, relationNode), 0, Command.FILE_LOADED))
      case Command.SYNCH => refreshText()
    }
  }
  
  private def tagConn() {
    val word = getSelectedText
    if(word == null)
      return
    if(!Resource.contains(Resource.CONN_LIST, word))
      addWord(Resource.CONN_LIST, word)
  }
  
  private def tagChainBeginning() {
    if(status != WAITING_FOR_B)
      return
    val caretPosition = getCaretPosition
    val word = getSelectedText
    if(word == null)
      return
    chainBuffer += word
    if(document.getText(getCaretPosition - word.length, word.length) == word)
      document.markHeadWord(getCaretPosition - word.length, word.length)
    else
      document.markHeadWord(getCaretPosition, word.length)
    select(-1, -1)
    status = EXPECTING_I
    setCaretPosition(caretPosition)
    ActionEventDispatcher.fireActionEvent(new ActionEvent(chainBuffer.mkString("-"), 0, Command.SET_STATUS))
  }
  
  private def tagChainIntermediate() {
    if(status != EXPECTING_I)
      return
    val caretPosition = getCaretPosition
    val word = getSelectedText
    if(word == null)
      return
    
    chainBuffer += word
    if(document.getText(getCaretPosition - word.length, word.length) == word)
      document.markTailWord(getCaretPosition - word.length, word.length)
    else
      document.markTailWord(getCaretPosition, word.length)
    select(-1, -1)
    setCaretPosition(caretPosition)
    ActionEventDispatcher.fireActionEvent(new ActionEvent(chainBuffer.mkString("-"), 0, Command.SET_STATUS))
  }
  
  private def tagChainEnding() {
    if(status != EXPECTING_I)
      return
    val caretPosition = getCaretPosition
    val word = getSelectedText
    if(word == null)
      return
    status = WAITING_FOR_B
    chainBuffer += word
    if(document.getText(getCaretPosition - word.length, word.length) == word)
      document.markTailWord(getCaretPosition - word.length, word.length)
    else
      document.markTailWord(getCaretPosition, word.length)
    select(-1, -1)
    setCaretPosition(caretPosition)
    val string = chainBuffer.mkString("-")
    if(Resource.indexOfChain(Resource.chainList, string) == -1)
      addChain(Resource.CHAIN_LIST, string)
    chainBuffer.clear
    ActionEventDispatcher.fireActionEvent(new ActionEvent("", 0, Command.SET_STATUS))
  }
  
  private def abort() {
    status = WAITING_FOR_B
    chainBuffer.clear
    ActionEventDispatcher.fireActionEvent(new ActionEvent("", 0, Command.SET_STATUS))
    document.tag()
    ActionEventDispatcher.fireActionEvent(new ActionEvent(FileLoadedMessage(LexicalChainPosition(null, fileName, getCaretPosition, -1), null), 0, Command.FILE_LOADED))
  }
  
  private def deleteWord() {
    val word = getSelectedText
    if(word == null)
      return
    val selectedColor = document.getColorAtPosition(getSelectionStart)
    if(selectedColor == Resource.getColor("connFg"))
      removeWord(Resource.CONN_LIST, word)
  }
  
  private def addLexicalChainInstance() {
    if(getSelectedText == null || Resource.getTree.getModel.asInstanceOf[RSTTreeModel].isSpanMarked(getSelectionStart, getSelectionEnd))
      return
    val model = Resource.getTree.getModel.asInstanceOf[RSTTreeModel]
    val root = model.getRoot.asInstanceOf[RSTRootNode]
    val newNode = new LexicalChainInstanceNode(root.UNDEFINED_ROLE, getSelectionStart, getSelectionEnd, model, fileName)
    newNode.refresh()
    root.addChild(newNode)
    ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
    ActionEventDispatcher.fireActionEvent(new ActionEvent(newNode, 0, Command.LEXICAL_CHAIN_INSTANCE_ADDED))
  }
  
  private def deleteSpan() {
    if(getSelectedText == null || Resource.getTree.getModel.asInstanceOf[RSTTreeModel].isSpanMarked(getSelectionStart, getSelectionEnd))
      return
    val start = getSelectionStart
    val length = {
      if((start == 0 || getText(start - 1, 1) == "\n") && (getSelectionEnd + 1 < getTextLength && getText(getSelectionEnd, 1) == "\n"))
        getSelectionEnd - getSelectionStart + 1
      else
        getSelectionEnd - getSelectionStart
    }
    document.deleteSpan(start, length)
  }
  
  private def getTextLength: Int = getText.length - document.getDefaultRootElement.getElementCount + 1
  
  def mouseClicked(e: MouseEvent): Unit = {}

  def mouseEntered(e: MouseEvent): Unit = {}

  def mouseExited(e: MouseEvent): Unit = {}

  def mousePressed(e: MouseEvent): Unit = {
    if(getSelectedText != null) {
      if(status == WAITING_FOR_B){
        if(!document.isPlainSpan(getSelectionStart, getSelectionEnd) &&
            document.getColorAtPosition(getSelectionStart) != document.headFg &&
            document.getColorAtPosition(getSelectionStart) != document.tailFg)
          deleteWordItem.setEnabled(true)
        else
          deleteWordItem.setEnabled(false)
        
        if(!Resource.getTree.getModel.asInstanceOf[RSTTreeModel].isSpanMarked(getSelectionStart, getSelectionEnd)) {
          addLexicalChainItem.setEnabled(true)
          deleteSpanItem.setEnabled(true)
        }
        else {
          addLexicalChainItem.setEnabled(false)
          deleteSpanItem.setEnabled(false)
        }
        
        setComponentPopupMenu(mainMenu)
      }
      else
        setComponentPopupMenu(chainMenu)
    }
    else {
      setComponentPopupMenu(null)
    }
  }

  def mouseReleased(e: MouseEvent): Unit = {}

  def keyReleased(e: KeyEvent): Unit = {
    e.getKeyCode match {
      case KeyEvent.VK_C => tagConn()
      case KeyEvent.VK_B => tagChainBeginning()
      case KeyEvent.VK_I => tagChainIntermediate()
      case KeyEvent.VK_E => tagChainEnding()
      case KeyEvent.VK_A => abort()
      case KeyEvent.VK_L => addLexicalChainInstance()
      case KeyEvent.VK_D => deleteWord()
      case KeyEvent.VK_DELETE => deleteSpan()
      case _ =>
    }
  }

  def keyPressed(e: KeyEvent): Unit = {}
  def keyTyped(e: KeyEvent): Unit = {}
}
