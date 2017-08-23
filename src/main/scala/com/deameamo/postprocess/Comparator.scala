package com.deameamo.postprocess

import java.io.{File, PrintWriter}
import java.text.DecimalFormat
import javax.xml.parsers.DocumentBuilderFactory

import com.deameamo.util.FileUtil
import org.w3c.dom.{Document, Element}

import scala.collection.mutable

object Comparator {

  def main(args: Array[String]) {
    val comparator = new Comparator
    comparator.compare("compare_1/tags_fei/done", "compare_1/tags_fei/tree/", "compare_1/tags_hou/tree/", "compare_1/result/")
    println("done")
  }
}

class Comparator {
  
  private val builder = DocumentBuilderFactory.newInstance.newDocumentBuilder
  
  var numberOfEleRelA = 0
  var numberOfEleRelB = 0
  
  var numberOfComparableEleRel = 0
  var numberOfIdenticalEleRel = 0
  var numberOfDiffCatEleRel = 0
  var numberOfDiffRoleEleRel = 0
  val sameCatEleRelMap = new mutable.HashMap[String, Info]
  val diffCatEleRelMap = new mutable.HashMap[String, Info]
  
  var numberOfConsistentDocs = 0
  var numberOfComRel = 0
  var numberOfIdenticalComRel = 0
  var numberOfDiffCatComRel = 0
  var numberOfDiffRoleComRel = 0
  val sameCatComRelMap = new mutable.HashMap[String, Info]
  val diffCatComRelMap = new mutable.HashMap[String, Info]
  
  val relAMap = new mutable.HashMap[String, Info]
  val relBMap = new mutable.HashMap[String, Info]
  
  val df = new DecimalFormat(".00")

  def compare(fileListPath: String, dirAPath: String, dirBPath: String, resultDirPath: String) {
    val allOut = FileUtil.getWriter(resultDirPath + "all")
    val comOut = FileUtil.getWriter(resultDirPath + "composite")
    val eleOut = FileUtil.getWriter(resultDirPath + "elementary")
    val list = FileUtil.readFile(fileListPath)
    list.foreach(path => {
      compareForPair(path, builder.parse(new File(dirAPath + path)), builder.parse(new File(dirBPath + path)), comOut, eleOut)
    })
    
    allOut.println("Stats for elementary relation:")
    allOut.println(s"numberOfEleRelA=$numberOfEleRelA")
    allOut.println(s"numberOfEleRelB=$numberOfEleRelB")
    allOut.println()
    
    allOut.println(s"numberOfComparableEleRel=$numberOfComparableEleRel")
    allOut.println(s"numberOfComparableEleRel/numberOfEleRelA=${calPercentage(numberOfComparableEleRel, numberOfEleRelA)}")
    allOut.println(s"numberOfComparableEleRel/numberOfEleRelA=${calPercentage(numberOfComparableEleRel, numberOfEleRelB)}")
    allOut.println(s"numberOfIdenticalEleRel=$numberOfIdenticalEleRel")
    allOut.println(s"numberOfDiffCatEleRel=$numberOfDiffCatEleRel")
    allOut.println(s"numberOfDiffRoleEleRel=$numberOfDiffRoleEleRel")
    allOut.println(s"numberOfIdenticalEleRel/numberOfComparableEleRel=${calPercentage(numberOfIdenticalEleRel, numberOfComparableEleRel)}")
    allOut.println(s"numberOfDiffCatEleRel/numberOfComparableEleRel=${calPercentage(numberOfDiffCatEleRel, numberOfComparableEleRel)}")
    allOut.println(s"numberOfDiffRoleEleRel/numberOfComparableEleRel=${calPercentage(numberOfDiffRoleEleRel, numberOfComparableEleRel)}")
    allOut.println(s"numberOfConsistentRoleEleRel/numberOfComparableEleRel=${calPercentage(numberOfIdenticalEleRel + numberOfDiffCatEleRel + numberOfDiffRoleEleRel, numberOfComparableEleRel)}")
    allOut.println()
    allOut.println("sameCatEleRelMap:")
    printMap(sameCatEleRelMap, allOut, printId = false)
    allOut.println()
    allOut.println("diffCatEleRelMap:")
    printMap(diffCatEleRelMap, allOut, printId = true)
    
    allOut.println()
    allOut.println("Stats for composite relation:")
    allOut.println(s"numberOfConsistentDocs=$numberOfConsistentDocs")
    allOut.println(s"numberOfConsistentDocs/documents=${calPercentage(numberOfConsistentDocs, list.size)}")
    allOut.println(s"numberOfComRel=$numberOfComRel")
    allOut.println(s"numberOfIdenticalComRel=$numberOfIdenticalComRel")
    allOut.println(s"numberOfDiffCatComRel=$numberOfDiffCatComRel")
    allOut.println(s"numberOfDiffRoleComRel=$numberOfDiffRoleComRel")
    allOut.println(s"numberOfIdenticalComRel/numberOfComRel=${calPercentage(numberOfIdenticalComRel, numberOfComRel)}")
    allOut.println(s"numberOfDiffCatComRel/numberOfComRel=${calPercentage(numberOfDiffCatComRel, numberOfComRel)}")
    allOut.println(s"numberOfDiffRoleComRel/numberOfComRel=${calPercentage(numberOfDiffRoleComRel, numberOfComRel)}")
    allOut.println(s"numberOfConsistentRoleEleRel/numberOfComRel=${calPercentage(numberOfIdenticalComRel + numberOfDiffCatComRel + numberOfDiffRoleComRel, numberOfComRel)}")
    allOut.println()
    allOut.println("sameCatComRelMap:")
    printMap(sameCatComRelMap, allOut, printId = false)
    allOut.println()
    allOut.println("diffCatComRelMap:")
    printMap(diffCatComRelMap, allOut, printId = false)
    
    allOut.println()
    allOut.println("Relation Rank:")
    printMap(relAMap, allOut, printId = false)
    allOut.println()
    printMap(relBMap, allOut, printId = false)
    
    FileUtil.closeWriter(allOut)
    FileUtil.closeWriter(comOut)
    FileUtil.closeWriter(eleOut)
  }
  
  def calPercentage(numerator: Int, denominator: Int): String = df.format(numerator.toDouble / denominator.toDouble * 100) + "%"
  
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
  
  def compareForPair(id: String, docA: Document, docB: Document, comOut: PrintWriter, eleOut: PrintWriter) {
    checkElementaryRelations(id, docA, docB, eleOut)
    checkComplexRelations(id, docA, docB, comOut)
  }
  
  def checkComplexRelations(id: String, docA: Document, docB: Document, comOut: PrintWriter) {
    val list = new mutable.MutableList[ComplexRelationPair]
    val consistent = checkComplexRelationsForElement(docA.getDocumentElement.getFirstChild.asInstanceOf[Element], docB.getDocumentElement.getFirstChild.asInstanceOf[Element], comOut, list)
    if(consistent) {
      comOut.println(id)
      numberOfConsistentDocs += 1
      list.foreach(pair => {
        if(pair.composite) {
          numberOfComRel += 1
//          println(s"${id}: A: ${pair.aRel.category}, B: ${pair.bRel.category}")
          if(pair.aRel.category == pair.bRel.category) {
            if(!sameCatComRelMap.contains(pair.aRel.category)) {
              sameCatComRelMap.put(pair.aRel.category, Info(pair.aRel.category, 0))
            }
            sameCatComRelMap.apply(pair.aRel.category).count += 1
            if(pair.aRel.role == pair.bRel.role) {
              numberOfIdenticalComRel += 1
            }
            else {
              numberOfDiffRoleComRel += 1
            }
          }
          else {
            val vs = pair.aRel.category + " vs. " + pair.bRel.category
            if(!diffCatComRelMap.contains(vs)) {
              diffCatComRelMap.put(vs, Info(vs, 0))
            }
            diffCatComRelMap.apply(vs).count += 1
            if(pair.aRel.role == pair.bRel.role) {
              numberOfDiffCatComRel += 1
            }
          }
        }
      })
    }
  }
  
  def checkComplexRelationsForElement(eleA: Element, eleB: Element, comOut: PrintWriter, list: mutable.MutableList[ComplexRelationPair]): Boolean = {
    if(eleA.getTagName != eleB.getTagName)
      return false
    if(eleA.getChildNodes.getLength != eleB.getChildNodes.getLength)
      return false
    if(eleA.getTagName == "chain")
      return true
    
    var complex = false
    var consistent = true
    var i = 0
    while(i < eleA.getChildNodes.getLength && consistent) {
      val childA = eleA.getChildNodes.item(i).asInstanceOf[Element]
      val childB = eleB.getChildNodes.item(i).asInstanceOf[Element]
      if(childA.getTagName == "relation")
        complex = true
      consistent = checkComplexRelationsForElement(childA, childB, comOut, list)
      i += 1
    }
    list += ComplexRelationPair(Relation(eleA), Relation(eleB), complex)
    consistent
  }
  
  def checkElementaryRelations(id: String, docA: Document, docB: Document, eleOut: PrintWriter) {
    val listA = new mutable.MutableList[ElementaryRelation]
    collectElementaryRelations(docA.getDocumentElement.getFirstChild.asInstanceOf[Element], listA, relAMap)
    val listB = new mutable.MutableList[ElementaryRelation]
    collectElementaryRelations(docB.getDocumentElement.getFirstChild.asInstanceOf[Element], listB, relBMap)
    var numberOfComparable = 0
    for(i <- listA.indices) {
      val relA = listA.apply(i)
      for(j <- listB.indices) {
        val relB = listB.apply(j)
        if(relA.isComparableWith(relB)) {
          numberOfComparable += 1
          if(relA.category == relB.category) {
            if(!sameCatEleRelMap.contains(relA.category)) {
              sameCatEleRelMap.put(relA.category, Info(relA.category, 0))
            }
//            println(relA.category)
            sameCatEleRelMap.apply(relA.category).count += 1
          }
          else {
            val pair = relA.category + " vs. " + relB.category
            if(!diffCatEleRelMap.contains(pair)) {
              diffCatEleRelMap.put(pair, Info(pair, 0))
            }
//            println(pair)
            diffCatEleRelMap.apply(pair).count += 1
            diffCatEleRelMap.apply(pair).list += id + ":" + relA.leftChain.startOffset
          }
          numberOfComparableEleRel += 1
          if(relA.isIdenticalTo(relB)) {
//            println(s"identical: ${relA.category}")
            numberOfIdenticalEleRel += 1
          }
          if(relA.hasDifferentCategoryWith(relB)) numberOfDiffCatEleRel += 1
          if(relA.hasDifferentChainRolesWith(relB)) numberOfDiffRoleEleRel += 1
        }
      }
    }
    numberOfEleRelA += listA.size
    numberOfEleRelB += listB.size
//    eleOut.println(s"${id}: ${listA.size}:${listB.size}:${numberOfComparable}")
    eleOut.println(s"${listA.size}/${listB.size}/$numberOfComparable")
  }
  
  def collectElementaryRelations(ele: Element, list: mutable.MutableList[ElementaryRelation], map: mutable.HashMap[String, Info]) {
    if(ele.getTagName == "relation") {
      val leftChild = ele.getFirstChild.asInstanceOf[Element]
      val rightChild = ele.getLastChild.asInstanceOf[Element]
      val category = ele.getAttribute("category")
      if(!map.contains(category)) {
        map.put(category, Info(category, 0))
      }
      map.apply(category).count += 1
      if(leftChild.getTagName == "chain" && rightChild.getTagName == "chain") {
        val rel = ElementaryRelation(ele.getAttribute("category"))
        rel.addChain(Chain(leftChild))
        rel.addChain(Chain(rightChild))
        list += rel
      }
      else {
        collectElementaryRelations(leftChild, list, map)
        collectElementaryRelations(rightChild, list, map)
      }
    }
  }
  
  case class ComplexRelationPair(aRel: Relation, bRel: Relation, composite: Boolean)
  
  case class ElementaryRelation(category: String) {
    
    var leftChain: Chain = _
    var rightChain: Chain = _
    
    def addChain(chain: Chain) {
      if(leftChain == null)
        leftChain = chain
      else {
        if(leftChain.startOffset < chain.startOffset) {
          rightChain = chain
        }
        else {
          rightChain = leftChain
          leftChain = chain
        }
      }
    }
    
    def isComparableWith(another: ElementaryRelation): Boolean = this.leftChain.startOffset == another.leftChain.startOffset && this.rightChain.startOffset == another.rightChain.startOffset
    
    def isIdenticalTo(another: ElementaryRelation): Boolean = this.category == another.category && this.leftChain.isIdenticalTo(another.leftChain) && this.rightChain.isIdenticalTo(another.rightChain)
    
    def hasDifferentCategoryWith(another: ElementaryRelation): Boolean = this.category != another.category && this.leftChain.isIdenticalTo(another.leftChain) && this.rightChain.isIdenticalTo(another.rightChain)
    
    def hasDifferentChainRolesWith(another: ElementaryRelation): Boolean = this.category == another.category && isComparableWith(another: ElementaryRelation) && this.leftChain.role != another.leftChain.role
  }
  
  case class Relation(element: Element) {
    val category: String = element.getAttribute("category")
    val role: Int = element.getAttribute("role").toInt
  }
  
  case class Chain(element: Element) {
    val startOffset: Int = element.getAttribute("startOffset").toInt
    val endOffset: Int = element.getAttribute("endOffset").toInt
    val role: String = element.getAttribute("role")
    
    def isIdenticalTo(another: Chain): Boolean = this.startOffset == another.startOffset && this.role == another.role
  }
  
  case class Info(string: String, var count: Int) {
    val list = new mutable.MutableList[String]
  }
}