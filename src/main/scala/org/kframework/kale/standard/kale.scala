package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._

import scala.language.implicitConversions

trait FreeLabel

case class Sort(name: String) extends kale.Sort

object Sort {
  object K extends Sort("K")
}

case class FreeLabel0 private(name: String)(implicit val env: Environment) extends Label0 with FreeLabel {
  def apply(): Term = FreeNode0(this)
}

case class FreeLabel1 private(name: String)(implicit val env: Environment) extends Label1 with FreeLabel {
  def apply(_1: Term): Term = FreeNode1(this, _1)
}

case class FreeLabel2 private(name: String)(implicit val env: Environment) extends Label2 with FreeLabel {
  def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
}

case class FreeLabel3 private(name: String)(implicit val env: Environment) extends Label3 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term): Term = FreeNode3(this, _1, _2, _3)
}

case class FreeLabel4 private(name: String)(implicit val env: Environment) extends Label4 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term = FreeNode4(this, _1, _2, _3, _4)
}

case class FreeNode0(label: Label0) extends Node0

case class FreeNode1(label: Label1, _1: Term) extends Node1

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2

case class FreeNode3(label: Label3, _1: Term, _2: Term, _3: Term) extends Node3

case class FreeNode4(label: Label4, _1: Term, _2: Term, _3: Term, _4: Term) extends Node4

case class FreeNode5(label: Label5, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term) extends Node5

case class FreeNode6(label: Label6, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term) extends Node6
