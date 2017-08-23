package com.deameamo.rst

import com.deameamo.swingx.ResizableBox

class TreePanel extends ResizableBox(ResizableBox.VERTICAL, false) {
  val treeBox = new TreeBox
  
  addItem(treeBox)
  
}