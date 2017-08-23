package com.deameamo.rst

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.awt.dnd._
import javax.swing.tree.TreePath

import com.deameamo.event.ActionEventDispatcher

class RSTTreeDragSource(tree: RSTTree) extends DragSource with DragSourceListener with DragGestureListener {
  
  var selectedNode: RSTNode = _

  createDefaultDragGestureRecognizer(tree, DnDConstants.ACTION_MOVE, this)
  
  def dragGestureRecognized(dge: DragGestureEvent) {
    val path = tree.getSelectionPath
    if(path == null || path.getPathCount <= 1) return
    selectedNode = path.getLastPathComponent.asInstanceOf[RSTNode]
    startDrag(dge, DragSource.DefaultCopyDrop, new TransferableTreeNode(path), this)
  }
  
  def dragEnter(event: DragSourceDragEvent) {}
  def dragExit(event: DragSourceEvent) {}
  def dragOver(event: DragSourceDragEvent) {}
  def dropActionChanged(event: DragSourceDragEvent) {}
  def dragDropEnd(event: DragSourceDropEvent) {}
}

class RSTTreeDropTarget(tree: RSTTree) extends DropTargetListener {
  
  val target = new DropTarget(tree, this)
  
  def dragEnter(event: DropTargetDragEvent) {}
  def dragExit(event: DropTargetEvent) {}
  def dragOver(event: DropTargetDragEvent) {}
  def dropActionChanged(event: DropTargetDragEvent) {}
  
  def drop(event: DropTargetDropEvent) {
    val point = event.getLocation
    val targetPath = tree.getClosestPathForLocation(point.x, point.y)
    val target = targetPath.getLastPathComponent.asInstanceOf[RSTNode]

    val transferable = event.getTransferable
    val flavors = transferable.getTransferDataFlavors
    for(flavor <- flavors) {
      if(transferable.isDataFlavorSupported(flavor)) {
        event.acceptDrop(DnDConstants.ACTION_MOVE)
        val path = transferable.getTransferData(flavor).asInstanceOf[TreePath]
        val source = path.getLastPathComponent.asInstanceOf[RSTNode]
        if(target.canAddChild(source)) {
          source.getParent.asInstanceOf[RSTNode].removeChild(source)
          target.addChild(source)
          tree.selectNode(target)
          ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
          event.dropComplete(true)
        }
        else {
          if(source != target && (
            (source.isInstanceOf[LexicalChainInstanceNode] && target.isInstanceOf[LexicalChainInstanceNode] && !source.getParent.isInstanceOf[RawNode] && !target.getParent.isInstanceOf[RawNode])
            || {
            if(source.getParent == target.getParent) {
              source.getParent match {
                case _: RawNode => false
                case _: RelationNode => true
                case _ => (source.isInstanceOf[LexicalChainInstanceNode] && target.isInstanceOf[LexicalChainInstanceNode]) || (source.isInstanceOf[RelationNode] && target.isInstanceOf[RelationNode])
              }
            }
            else
              false
          })) {
//            println(s"${source.getParent.isInstanceOf[RawNode]}")
            source.asInstanceOf[RSTNode].switchPosition(target.asInstanceOf[RSTNode])
            tree.repaint()
            ActionEventDispatcher.fireActionEvent(Command.TREE_MODEL_CHANGED)
            event.dropComplete(true)
          }
        }
      }
    }
  }
}

class TransferableTreeNode(path: TreePath) extends Transferable {
  val flavors = new Array[DataFlavor](1)
  flavors.update(0, new DataFlavor("class/javax.swing.tree.treePath"))
  
  def getTransferDataFlavors: Array[DataFlavor] = flavors

  def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
    flavor.getSubType match {
      case "javax.swing.tree.treepath" => true
      case _ => false
    }
  }

  def getTransferData(flavor: DataFlavor): Object = {
    flavor.getSubType match {
      case "javax.swing.tree.treepath" => path
      case _ => null
    }
  }
}