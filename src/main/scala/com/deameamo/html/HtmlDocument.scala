package com.deameamo.html

import java.io.{File, FileInputStream}
import java.util.Properties

import com.deameamo.rst.Resource

import scala.collection.mutable.ListBuffer

object HtmlDocument {
  def main(args: Array[String]): Unit = {
    val doc = new HtmlDocument
    doc.addMeta("meta1")
    doc.addMeta("meta2")
    doc.addLink("link1")
    doc.addScript("script1")
    doc.title = "title1"
    println(doc)
    load(Resource.DEFAULT_PARAMS_PATH)
  }

  def load(paramsPath: String): Unit = {
    import com.deameamo.rst.Resource._
    val props = new Properties()
    props.load(new FileInputStream(new File(paramsPath)))
    val names = props.propertyNames()
    while(names.hasMoreElements) {
      println(names.nextElement())

    }
  }
}

class HtmlDocument {

  private val metas = new ListBuffer[Meta]

  private val links = new ListBuffer[Link]

  private val scripts = new ListBuffer[Script]

  private var title: String = _

  private val body = new HtmlElement("body")

  def addMeta(value: String): Unit = {
    metas += Meta(value)
  }

  def addLink(value: String): Unit = {
    links += Link(value)
  }

  def addScript(value: String): Unit = {
    scripts += Script(value)
  }

  override def toString: String = {
    s"""<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
      |<html xmlns="http://www.w3.org/1999/xhtml">
      |<head>\n""".stripMargin +
      {if(metas.nonEmpty) metas.mkString("\n") + "\n" else ""} +
      {if(links.nonEmpty) links.mkString("\n") + "\n" else ""} +
      {if(scripts.nonEmpty) scripts.mkString("\n") + "\n" else ""} +
      s"<title>$title</title>\n" +
      s"</head>\n" +
      s"$body" +
      s"</html>"
  }

  case class Meta(value: String) {
    override def toString: String = s"<meta $value/>"
  }

  case class Link(value: String) {
    override def toString: String = s"""<link href="$value" rel="stylesheet" type="text/css"/>"""
  }

  case class Script(value: String) {
    override def toString: String = s"""<script src="$value"/>"""
  }

}

class HtmlElement(tag: String) {

  val attrs = new ListBuffer[Attr]

  val children = new ListBuffer[HtmlElement]

  var text: String = ""

  def addAttr(name: String, value: String): Unit = {
    attrs += Attr(name, value)
  }

  def addChild(child: HtmlElement): Unit = {
    children += child
  }

  override def toString: String = {
    s"<$tag${if(attrs.nonEmpty) attrs.mkString(" ", " ", "") else ""}>\n" + {
      if(children.nonEmpty) {
        children.mkString("\n") + s"</$tag>\n"
      } else {
        s"$text\n</$tag>\n"
      }
    }
  }

  case class Attr(name: String, value: String) {
    override def toString: String = {
      s"""$name="$value""""
    }
  }
}
