package com.github.dmlap

sealed trait Node {
  def asString: String
}
trait Elem extends Node {
  val name: String
  def attributes: Map[String, String] = Map.empty
  def children: List[Node] = Nil
  def asString: String = 
    "<" + name + (attributes.iterator.foldLeft("") { (result, attr) =>
      result + " " + attr._1 + "=\"" + attr._2 + "\""
    }) + ">" +
      children.map(_.asString).mkString("") +
    "</" + name + ">"
  def copy(name: String = this.name,
           attributes: Map[String, String] = this.attributes,
           children: List[Node] = this.children) = {
    val _name = name
    val _attributes = attributes
    val _children = children
    new Elem {
      override val name = _name
      override val attributes = _attributes
      override val children = _children
    }
  }
}
trait Text extends Node {
  val text: String
  def asString: String = text
  def copy(text: String = this.text) = {
    val _text = text
    new Text {
      val text = _text
    }
  }
}
