package com.github.dmlap

import org.specs2.mutable._

object NodeSpecs extends Specification {
  "Elem" should {
    "serialize its name" in {
      object Element extends Elem {
        val name = "elem"
      }
      Element.asString must_== "<elem></elem>"
    }
    "serialize its attributes" in {
      object Element extends Elem {
        val name = "elem"
        override val attributes = Map("attr0" -> "value0", "attr1" -> "value1")
      }
      Element.asString must startWith("<elem ")
      Element.asString must contain("attr0=\"value0\"")
      Element.asString must contain("attr1=\"value1\"")
      Element.asString must endWith("></elem>")
    }
    "serialize its children" in {
      object Element extends Elem {
        val name = "elem"
        object Child0 extends Elem {
          val name = "child0"
        }
        object Child1 extends Elem {
          val name = "child1"
        }
        override val children = List(Child0, Child1)
      }
      Element.asString must_== "<elem><child0></child0><child1></child1></elem>"
    }
    "have a functional copy constructor" in {
      object Element extends Elem {
        val name = "elem"
      }
      val result = Element.copy()
      result must not eq(Element)
      result.children must_==(Element.children)
      result.attributes must_==(Element.attributes)
      result.name must_==(Element.name)
    }
    "have a copy constructor that supports overrides" in {
      object Element extends Elem {
        val name = "elem"
      }
      Element.copy(name = "elem0").name must_== "elem0"
    }
  }
  "Text" should {
    "serialize its contents" in {
      object Element extends Elem {
        val name = "elem"
        object Child0 extends Text {
          val text = "text0"
        }
        object Child1 extends Text {
          val text = "text1"
        }
        override val children = List(Child0, Child1)
      }
      Element.asString must_== "<elem>text0text1</elem>"
    }
    "have a functional copy constructor" in {
      object Txt extends Text {
        val text = "text"
      }
      val result = Txt.copy()
      result must not eq(Txt)
      result.text must_==(Txt.text)
    }
    "have a copy constructor that supports overrides" in {
      object Txt extends Text {
        val text = "text"
      }
      Txt.copy(text = "text0").text must_==("text0")
    }
  }
}
