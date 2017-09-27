package org.kframework.kale

trait FreeMixin extends Mixin {
  def FreeLabel0(name: String): FreeLabel0

  def FreeLabel1(name: String): FreeLabel1

  def FreeLabel2(name: String): FreeLabel2

  def FreeLabel3(name: String): FreeLabel3

  def FreeLabel4(name: String): FreeLabel4

  def FreeLabel5(name: String): FreeLabel5

  def FreeLabel6(name: String): FreeLabel6

  def FreeLabelN(name: String, arity: Int): FreeLabelN
}

trait Constructor extends NodeLabel

trait FreeLabel extends Constructor {
  val isPredicate = Some(false)
}

trait FreeLabel0 extends Label0 with FreeLabel {
  private lazy val uniqueInstance = FreeNode0(this)

  def apply(): Node0 = uniqueInstance
}

trait FreeLabel1 extends Label1 with FreeLabel {
  def apply(_1: Term): Term = env.bottomize(_1) {
    FreeNode1(this, _1)
  }
}

trait FreeLabel2 extends Label2 with FreeLabel {
  def apply(_1: Term, _2: Term): Term = env.bottomize(_1, _2) {
    FreeNode2(this, _1, _2)
  }
}

trait FreeLabel3 extends Label3 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term): Term = env.bottomize(_1, _2, _3) {
    FreeNode3(this, _1, _2, _3)
  }
}

trait FreeLabel4 extends Label4 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term = env.bottomize(_1, _2, _3, _4) {
    FreeNode4(this, _1, _2, _3, _4)
  }
}

trait FreeLabel5 extends Label5 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term): Term = env.bottomize(_1, _2, _3, _4, _5) {
    FreeNode5(this, _1, _2, _3, _4, _5)
  }
}

trait FreeLabel6 extends Label6 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term): Term = env.bottomize(_1, _2, _3, _4, _5, _6) {
    FreeNode6(this, _1, _2, _3, _4, _5, _6)
  }
}

trait FreeLabelN extends LabelN with FreeLabel {
  def apply(children: Seq[Term]): Term = env.bottomize(children: _*) {
    FreeNodeN(this, children)
  }
}

trait FreeNode // extends Node

case class FreeNode0(label: Label0) extends Node0 with FreeNode with Application

case class FreeNode1(label: Label1, _1: Term) extends Node1 with FreeNode with Application

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2 with FreeNode with Application

case class FreeNode3(label: Label3, _1: Term, _2: Term, _3: Term) extends Node3 with FreeNode with Application

case class FreeNode4(label: Label4, _1: Term, _2: Term, _3: Term, _4: Term) extends Node4 with FreeNode with Application

case class FreeNode5(label: Label5, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term) extends Node5 with FreeNode with Application

case class FreeNode6(label: Label6, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term) extends Node6 with FreeNode with Application

case class FreeNodeN(label: NodeLabel, children: Seq[Term]) extends NodeN with FreeNode with Application
