package org.kframework.kale


trait Constructor

trait FreeLabel extends Constructor

trait FreeLabel0 extends Label0 with FreeLabel {
  private lazy val uniqueInstance = FreeNode0(this)
  def apply(): Term = uniqueInstance
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

case class FreeNode0(label: Label0) extends Node0 with Application

case class FreeNode1(label: Label1, _1: Term) extends Node1 with Application

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2 with Application

case class FreeNode3(label: Label3, _1: Term, _2: Term, _3: Term) extends Node3 with Application

case class FreeNode4(label: Label4, _1: Term, _2: Term, _3: Term, _4: Term) extends Node4 with Application

case class FreeNode5(label: Label5, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term) extends Node5 with Application

case class FreeNode6(label: Label6, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term) extends Node6 with Application
