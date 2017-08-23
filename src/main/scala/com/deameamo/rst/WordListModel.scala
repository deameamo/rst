package com.deameamo.rst

import javax.swing.DefaultListModel

class WordListModel[E <: String](listName: String) extends DefaultListModel[String] {
  
  def load() {
    removeAllElements()
    for(d <- Resource.getWordList(listName)) {
      add(getSize, d)
    }
  }
  
  def moveUp(i: Int) {
    if(Resource.moveUpWord(listName, i + 1)) {
      set(i, Resource.getWordList(listName).get(i).get)
      set(i + 1, Resource.getWordList(listName).get(i + 1).get)
    }
  }
  
  def moveDown(i: Int) {
    if(Resource.moveDownWord(listName, i - 1)) {
      set(i - 1, Resource.getWordList(listName).get(i - 1).get)
      set(i, Resource.getWordList(listName).get(i).get)
    }
  }
  
  def removeWord(i: Int) {
    remove(i)
    Resource.removeWord(listName, i)
  }
  
  def addWord(word: String): Boolean = {
    val index = Resource.addWord(listName, word)
    if(index != -1) {
      insertElementAt(word, index)
      true
    }
    else
      false
  }
  
  def synch() {
    removeAllElements()
    for(d <- Resource.getWordList(listName)) {
      add(getSize, d)
    }
  }
  
}