package org.kframework.kale

import org.kframework.kale.builtin.{HasINT, HasINTdiv, MapLabel}
import org.kframework.kale.standard._

class IMPCommonSignature(implicit val env: Environment with HasINT with HasINTdiv) {
  import env._

  def lhs(t: Term): Term = t match {
    case Rewrite(l, r) => l
    case Node(label, children) => label(children.toSeq map lhs)
    case o => o
  }

  def rhs(t: Term): Term = t match {
    case Rewrite(l, r) => r
    case Node(label, children) => label(children.toSeq map rhs)
    case o => o
  }

  val div = FreeLabel2("_/_")
  val plus = FreeLabel2("_+_")
  val leq = FreeLabel2("_<=_")
  val not = FreeLabel1("!_")
  val and = FreeLabel2("_&&_")
  val emptyBlock = FreeLabel0("{}")
  val block = FreeLabel1("{_}")
  val assign = FreeLabel2("_:=_")
  val ifthenelse = FreeLabel3("if_then_else_")
  val whiledo = FreeLabel2("while(_)_")
  val seq = FreeLabel2("__")
  val program = FreeLabel2("_;_")

  val T = FreeLabel2("<T>")
  val k = FreeLabel1("<k>")
  val state = FreeLabel1("<state>")

  val varBinding = FreeLabel2("_->_")

  val emptyIntList = FreeLabel0(".List{Int}")

  val emptyStates = FreeLabel0("emptyStates")

  val statesMap = MapLabel("_StatesMap_", {
    case varBinding(variable, _) => variable
  }, emptyStates())

  val emptyk = FreeLabel0(".K")
}
