package com.deameamo.rst

import javax.swing.DefaultListModel

class ChainListModel[E <: String](listName: String) extends DefaultListModel[String] {
  
  def load() {
    removeAllElements()
    for(chain <- Resource.getChainList(listName)) {
      add(getSize, chain.string)
    }
  }
  
  def moveUp(i: Int) {
    if(Resource.moveUpChain(listName, i + 1)) {
      set(i, Resource.getChainList(listName).get(i).get.string)
      set(i + 1, Resource.getChainList(listName).get(i + 1).get.string)
    }
  }
  
  def moveDown(i: Int) {
    if(Resource.moveDownChain(listName, i - 1)) {
      set(i - 1, Resource.getChainList(listName).get(i - 1).get.string)
      set(i, Resource.getChainList(listName).get(i).get.string)
    }
  }
  
  def removeChain(i: Int) {
    remove(i)
    Resource.removeChain(listName, i)
  }
  
  def addChain(string: String): Boolean = {
    if(Resource.addChain(listName, string)) {
      removeAllElements()
      for(chain <- Resource.getChainList(listName)) {
        add(getSize, chain.string)
      }
      true
    }
    else
      false
  }
  
  def synch() {
    removeAllElements()
    for(chain <- Resource.getChainList(listName)) {
      add(getSize, chain.string)
    }
  }
  
}