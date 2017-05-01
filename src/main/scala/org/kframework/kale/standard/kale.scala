package org.kframework.kale.standard

import org.kframework.{kale, kore}
import org.kframework.kale._
import org.kframework.kore.Pattern

import scala.language.implicitConversions

trait Constructor

trait FreeLabel extends Constructor

case class Sort(name: String) extends kale.Sort

object Sort {

  object K extends Sort("K")

}

case class SimpleFreeLabel0 private(name: String)(implicit val env: Environment) extends Label0 with FreeLabel {
  private lazy val uniqueInstance = FreeNode0(this)
  def apply(): Term = uniqueInstance
}

case class SimpleFreeLabel1 private(name: String)(implicit val env: Environment) extends Label1 with FreeLabel {
  def apply(_1: Term): Term = env.bottomize(_1) {
    FreeNode1(this, _1)
  }
}

case class SimpleFreeLabel2 private(name: String)(implicit val env: Environment) extends Label2 with FreeLabel {
  def apply(_1: Term, _2: Term): Term = env.bottomize(_1, _2) {
    FreeNode2(this, _1, _2)
  }
}

case class SimpleFreeLabel3 private(name: String)(implicit val env: Environment) extends Label3 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term): Term = env.bottomize(_1, _2, _3) {
    FreeNode3(this, _1, _2, _3)
  }
}

case class SimpleFreeLabel4 private(name: String)(implicit val env: Environment) extends Label4 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term = env.bottomize(_1, _2, _3, _4) {
    FreeNode4(this, _1, _2, _3, _4)
  }
}

case class FreeNode0(label: Label0) extends Node0 with Application

case class FreeNode1(label: Label1, _1: Term) extends Node1 with Application

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2 with Application

case class FreeNode3(label: Label3, _1: Term, _2: Term, _3: Term) extends Node3 with Application

case class FreeNode4(label: Label4, _1: Term, _2: Term, _3: Term, _4: Term) extends Node4 with Application

case class FreeNode5(label: Label5, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term) extends Node5 with Application

case class FreeNode6(label: Label6, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term) extends Node6 with Application
