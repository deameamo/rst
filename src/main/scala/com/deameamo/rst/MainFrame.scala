package com.deameamo.rst

import java.awt.{Color, EventQueue, Font}
import java.awt.event.{ActionEvent, ActionListener}
import java.io.File
import javax.swing._

import com.deameamo.swingx._
import com.deameamo.event.ActionEventDispatcher

object MainFrame {
  def main(args: Array[String]) {
    EventQueue.invokeLater(new Runnable {
      def run(): Unit = {
        val mainFrame = new MainFrame
        mainFrame.setVisible(true)
      }
    })
  }
}

class MainFrame extends DefaultMainFrame with ActionListener {

  val song16Font = new Font("宋体", Font.BOLD, 16)
  
  setTitle(new File(Resource.DEFAULT_PARAMS_PATH).getAbsolutePath)
  
  val menuBuilder = new MenuBuilder("com.deameamo.rst.MenuBar")
  val bar = new JMenuBar
  val fileMenu: JMenu = menuBuilder.createMenu("MenuFile", 'F')
  bar.add(fileMenu)
  fileMenu.add(menuBuilder.createMenuItem("ItemOpen", this, Command.OPEN, "ItemOpenAcc"))
  fileMenu.add(menuBuilder.createMenuItem("ItemSave", this, Command.SAVE, "ItemSaveAcc"))
  fileMenu.add(menuBuilder.createMenuItem("ItemExportRules", this, Command.EXPORT_RULES, "ItemExportRulesAcc"))
  fileMenu.add(menuBuilder.createMenuItem("ItemExportConjunctions", this, Command.EXPORT_CONJUNCTIONS, "ItemExportConjunctionsAcc"))
  fileMenu.add(menuBuilder.createMenuItem("ItemExit", this, Command.EXIT, "ItemExitAcc"))
  
  val editMenu: JMenu = menuBuilder.createMenu("MenuEdit", 'E')
  bar.add(editMenu)
  editMenu.add(menuBuilder.createMenuItem("ItemStandardize", this, Command.STANDARDIZE))
  
  setJMenuBar(bar)
  
  val contentBox = new DockableBox 
  contentBox.setResizable(true)
  contentBox.contentBox.setBackground(new Color(0xE1E6F6))
  contentBox.contentBox.setBorderColor(new Color(0xE1E6F6))
  contentBox.contentBox.setBorderLength(5)
  val editorDockable = new HorizontalDockableItem(new EditorPanel, "Article", song16Font, "icons/document_icon_sm.png", "icons/document_icon.png")
  val treeDockable = new HorizontalDockableItem(new TreePanel, "RST Tree", song16Font, "icons/rst_icon_sm.png", "icons/rst_icon.png")
  val patternDockable = new HorizontalDockableItem(new LexicalChainPanel, "Lexical Chain", song16Font, "icons/pattern_icon_sm.png", "icons/pattern_icon.png")
  val listDockable = new HorizontalDockableItem(new ListPanel, "Key Word List", song16Font, "icons/list_icon_sm.png", "icons/list_icon.png")
  
  editorDockable.addActionListener(this)
  treeDockable.addActionListener(this)
  patternDockable.addActionListener(this)
  listDockable.addActionListener(this)
  
  contentBox.addItem(editorDockable, 0.5)
  contentBox.addItem(treeDockable, 0.15)
  contentBox.addItem(patternDockable, 0.15)
  contentBox.addItem(listDockable, 0.2)
  
//  contentBox.dockItem(editorDockable)
//  contentBox.dockItem(treeDockable)
//  contentBox.dockItem(patternDockable)
//  contentBox.dockItem(listDockable)
  
  val statusFont = new Font("宋体", Font.PLAIN, 14)
  val statusBox = new ResizableBox
  statusBox.setBorder(BorderFactory.createLoweredBevelBorder())
  val statusLabel = new JLabel
  statusLabel.setFont(statusFont)
  statusBox.addItem(statusLabel)
  
  mainBox.setAlignment(ResizableBox.VERTICAL)
  mainBox.addItem(contentBox)
  mainBox.addRigidItem(statusBox, 25)
  val chooser = new JFileChooser
  chooser.addChoosableFileFilter(new SimpleFileFilter("prm", "Parameter Files"))
  
  maximize
  Resource.load(Resource.DEFAULT_PARAMS_PATH)
  ActionEventDispatcher.addActionListener(this, Command.SET_STATUS)
  
  def setStatus(text: String) {
    statusLabel.setText(text)
  }
  
  def actionPerformed(event: ActionEvent) {
    event.getActionCommand match {
      case Command.OPEN =>
        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")))
        val option = chooser.showOpenDialog(this)
        if(option == JFileChooser.APPROVE_OPTION) {
          Resource.load(chooser.getSelectedFile.getAbsolutePath)
          setTitle(chooser.getSelectedFile.getAbsolutePath)
        }
      case Command.SAVE => ActionEventDispatcher.fireActionEvent(Command.SAVE)
      case Command.STANDARDIZE =>
        Resource.standardize()
        JOptionPane.showMessageDialog(this, "Done")
      case Command.EXPORT_RULES =>
        Resource.exportRules()
        JOptionPane.showMessageDialog(this, "Done")
      case Command.EXPORT_CONJUNCTIONS =>
        Resource.exportConjunctions()
        JOptionPane.showMessageDialog(this, "Done")
      case Command.EXIT => System.exit(0)
      case Command.SET_STATUS => setStatus(event.getSource.asInstanceOf[String])
      case _ =>
    }
  }
}