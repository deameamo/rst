package com.deameamo.rst

import javax.swing.JList
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import com.deameamo.swingx.ResizableBox

class ListPanel extends ResizableBox(ResizableBox.VERTICAL) with ListSelectionListener {

  //  val keyList = new WordListEditor("关键词", Resource.KEY_LIST)
//  val auxList = new WordListEditor("辅助词", Resource.AUX_LIST)
  val chainList = new ChainListEditor("Predicates", Resource.CHAIN_LIST)
  val connList = new WordListEditor("Conjunctions", Resource.CONN_LIST)
      
  val listBox = new ResizableBox(ResizableBox.HORIZONTAL)
//  listBox.addItem(keyList, 0.25)
//  listBox.addItem(auxList, 0.25)
  listBox.addItem(chainList, 0.5)
  listBox.addItem(connList, 0.5)
  
  addItem(listBox)
  
//  keyList.wordList.addListSelectionListener(this)
//  auxList.wordList.addListSelectionListener(this)
  chainList.chainList.addListSelectionListener(this)
  connList.wordList.addListSelectionListener(this)
  
  var selected: JList[String] = _
  
  def valueChanged(event: ListSelectionEvent): Unit = {
    val source = event.getSource.asInstanceOf[JList[String]]
    if(selected == null) {
      selected = source
    }
    else {
      if(source != selected) {
//        keyList.wordList.removeListSelectionListener(this)
//        auxList.wordList.removeListSelectionListener(this)
        chainList.chainList.removeListSelectionListener(this)
        connList.wordList.removeListSelectionListener(this)
        
        selected.clearSelection()
        selected = source 
        
//        keyList.wordList.addListSelectionListener(this)
//        auxList.wordList.addListSelectionListener(this)
        chainList.chainList.addListSelectionListener(this)
        connList.wordList.addListSelectionListener(this)
      }
    }
  }
  
}