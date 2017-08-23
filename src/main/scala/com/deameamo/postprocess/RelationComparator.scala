package com.deameamo.postprocess

import java.io.{File, PrintWriter}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}

import com.deameamo.util.FileUtil
import org.w3c.dom.{Document, Element}

import scala.collection.mutable

object RelationComparator {

  def main(args: Array[String]) {
    val comparator = new RelationComparator
    comparator.compare("compare_1/tags_fei/done", "compare_1/tags_fei/tree/", "compare_1/tags_hou/tree/", "compare_1/")
    println("done")
  }
}

class RelationComparator {
  
  val builder: DocumentBuilder = DocumentBuilderFactory.newInstance.newDocumentBuilder
  var count = 0
  var eleCount = 0
  var comCount = 0
  var allA = 0
  var allB = 0
  
  val sameCatEleRelMap = new mutable.HashMap[String, Info]
  val diffCatEleRelMap = new mutable.HashMap[String, Info]
  val sameCatComRelMap = new mutable.HashMap[String, Info]
  val diffCatComRelMap = new mutable.HashMap[String, Info]

  def compare(fileListPath: String, dirAPath: String, dirBPath: String, resultDirPath: String) {
    
    val out = FileUtil.getWriter(resultDirPath + "compareResult")
    
    val list = FileUtil.readFile(fileListPath)
    list.foreach(path => {
      compareForPair(path, builder.parse(new File(dirAPath + path)), builder.parse(new File(dirBPath + path)))
    })
    
    out.println(s"allA: $allA")
    out.println(s"allB: $allB")
    out.println(s"count: $count")
    out.println(s"eleCount: $eleCount")
    out.println(s"comCount: $comCount")
    out.println()
    
    out.println("sameCatEleRelMap:")
    printMap(sameCatEleRelMap, out, printId = true)
    out.println()
    out.println("diffCatEleRelMap:")
    printMap(diffCatEleRelMap, out, printId = true)
    out.println()
    out.println("sameCatComRelMap:")
    printMap(sameCatComRelMap, out, printId = true)
    out.println()
    out.println("diffCatComRelMap:")
    printMap(diffCatComRelMap, out, printId = true)
    out.println()
    
    FileUtil.closeWriter(out)
  }
  
  def compareForPair(id: String, docA: Document, docB: Document) {
    val listA = getRelationElements(docA)
    val listB = getRelationElements(docB)
    allA += listA.size
    allB += listB.size
    for(i <- listA.indices) {
      val relA = listA.apply(i)
      for(j <- listB.indices) {
        val relB = listB.apply(j)
        if(isRelationComparable(relA, relB)) {
          count += 1
          if(isElementary(relA)) {
            if(relA.getAttribute("category") == relB.getAttribute("category")) {
              addToMap(sameCatEleRelMap, relA, id)
            }
            else {
              addToMap(diffCatEleRelMap, relA, relB, id)
            }
            eleCount += 1
          }
          else {
            if(relA.getAttribute("category") == relB.getAttribute("category")) {
              addToMap(sameCatComRelMap, relA, id)
            }
            else {
              addToMap(diffCatComRelMap, relA, relB, id)
            }
            comCount += 1
          }
        }
      }
    }
  }
  
  def getRelationElements(doc: Document): mutable.MutableList[Element] = {
    val list = new mutable.MutableList[Element]
    extractRelationElementsForElement(doc.getDocumentElement.getChildNodes.item(0).asInstanceOf[Element], list)
    list
  }

  def extractRelationElementsForElement(ele: Element, list: mutable.MutableList[Element]): Unit = {
    if(ele.getTagName == "relation") {
      list += ele
      for(i <- 0 until ele.getChildNodes.getLength) {
        val child = ele.getChildNodes.item(i).asInstanceOf[Element]
        if(child.getTagName == "relation")
          extractRelationElementsForElement(child, list)
      }
    }
  }
  
  def isElementary(ele: Element): Boolean = ele.getChildNodes.item(0).asInstanceOf[Element].getTagName == "chain" && ele.getChildNodes.item(1).asInstanceOf[Element].getTagName == "chain"
  
  def isRelationComparable(a: Element, b: Element): Boolean = {
    if(a.getTagName != b.getTagName)
      return false
    if(a.getTagName == "chain") {
      (a.getAttribute("startOffset").toInt == b.getAttribute("startOffset").toInt) && (a.getAttribute("endOffset").toInt == b.getAttribute("endOffset").toInt)
    }
    else {
      val a0 = a.getChildNodes.item(0).asInstanceOf[Element]
      val a1 = a.getChildNodes.item(1).asInstanceOf[Element]
      val b0 = b.getChildNodes.item(0).asInstanceOf[Element]
      val b1 = b.getChildNodes.item(1).asInstanceOf[Element]
      if((a0.getTagName == b0.getTagName) && (a1.getTagName == b1.getTagName)) {
        isRelationComparable(a0, b0) && isRelationComparable(a1, b1)
      }
      else
        false
    }
  }
  
  def addToMap(map: mutable.HashMap[String, Info], ele: Element, id: String) {
    val category = ele.getAttribute("category")
    if(!map.contains(category)) {
      map.put(category, Info(category, 0))
    }
    map.apply(category).count += 1
    map.apply(category).list += id + ":" + getLeftMostLeaf(ele).getAttribute("startOffset")
  }
  
  def addToMap(map: mutable.HashMap[String, Info], eleA: Element, eleB: Element, id: String) {
    val vs = eleA.getAttribute("category") + " vs. " + eleB.getAttribute("category")
    if(!map.contains(vs)) {
      map.put(vs, Info(vs, 0))
    }
    map.apply(vs).count += 1
    map.apply(vs).list += id + ":" + getLeftMostLeaf(eleA).getAttribute("startOffset")
  }
  
  def getLeftMostLeaf(ele: Element): Element = {
    if(ele.getTagName == "chain")
      ele
    else
      getLeftMostLeaf(ele.getChildNodes.item(0).asInstanceOf[Element])
  }
  
  def printMap(map: mutable.HashMap[String, Info], out: PrintWriter, printId: Boolean) {
    val list = new mutable.MutableList[Info]
    list ++= map.values
    val sorted = list.sortWith((a, b) => a.count >= b.count)
    var total = 0
    sorted.foreach(item =>{
      out.println(s"${item.string} (${item.count}): ${if(printId) item.list else ""} ")
      total += item.count
    })
    out.println(s"total: $total")
  }
  
  case class Info(string: String, var count: Int) {
    val list = new mutable.MutableList[String]
  }
}