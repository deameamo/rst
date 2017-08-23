package com.deameamo.rst

import java.awt.dnd._
import javax.swing.tree.{DefaultMutableTreeNode, TreePath}

class PatternTreeDragSource(tree: LexicalChainPatternTree) extends DragSource with DragSourceListener with DragGestureListener {
  
  var selectedNode: DefaultMutableTreeNode = _

  createDefaultDragGestureRecognizer(tree, DnDConstants.ACTION_MOVE, this)
  
  def dragGestureRecognized(dge: DragGestureEvent) {
    val path = tree.getSelectionPath
    if(path == null || path.getPathCount <= 1) return
    selectedNode = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
    startDrag(dge, DragSource.DefaultCopyDrop, new TransferableTreeNode(path), this)
  }
  
  def dragEnter(event: DragSourceDragEvent) {}
  def dragExit(event: DragSourceEvent) {}
  def dragOver(event: DragSourceDragEvent) {}
  def dropActionChanged(event: DragSourceDragEvent) {}
  def dragDropEnd(event: DragSourceDropEvent) {}
}

class PatternTreeDropTarget(tree: LexicalChainPatternTree) extends DropTargetListener {
  
  val target = new DropTarget(tree, this)
  
  def dragEnter(event: DropTargetDragEvent) {}
  def dragExit(event: DropTargetEvent) {}
  def dragOver(event: DropTargetDragEvent) {}
  def dropActionChanged(event: DropTargetDragEvent) {}
  
  def drop(event: DropTargetDropEvent) {
    val point = event.getLocation
    val targetPath = tree.getClosestPathForLocation(point.x, point.y)
    val target = targetPath.getLastPathComponent

    val transferable = event.getTransferable
    val flavors = transferable.getTransferDataFlavors
    for(flavor <- flavors) {
      if(transferable.isDataFlavorSupported(flavor)) {
        event.acceptDrop(DnDConstants.ACTION_MOVE)
        val path = transferable.getTransferData(flavor).asInstanceOf[TreePath]
        val source = path.getLastPathComponent
        target match {
          case patternTarget: LexicalChainPatternNode =>
            source match {
              case patternSource: LexicalChainPatternNode =>
                if (patternTarget.canAddChild(patternSource)) {
                  patternSource.getParent.asInstanceOf[LexicalChainPatternNode].removePatternNode(patternSource)
                  patternTarget.addPatternNode(patternSource)
                  patternTarget.model.notifyDataChanged()
                  patternTarget.model.checkForContradiction()
                }
              case chainSource: LexicalChainNode =>
                if (patternTarget.canAddChild(chainSource)) {
                  chainSource.getParent.asInstanceOf[LexicalChainPatternNode].removeChainNode(chainSource)
                  patternTarget.addChainNode(chainSource)
                  patternTarget.model.notifyDataChanged()
                  patternTarget.model.checkForContradiction()
                }
              case _ =>
            }
          case _ =>
            val chainTarget = target.asInstanceOf[LexicalChainNode]
            source match {
              case chainSource: LexicalChainNode =>
                if (chainTarget != chainSource && chainTarget.lexemes.equals(chainSource.lexemes)) {
                  chainTarget.addPositions(chainSource)
                  val chainSourceParent = chainSource.getParent.asInstanceOf[LexicalChainPatternNode]
                  chainSourceParent.removeChainNode(chainSource)
                  chainSourceParent.model.notifyDataChanged()
                  chainSourceParent.model.checkForContradiction()
                }
              case _ =>
            }
        }
        event.dropComplete(true)
      }
    }
  }
}