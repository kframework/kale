package org.kframework.kale

import org.kframework.kale.builtin.{importINT, MapLabel}
import org.kframework.kale.standard._

class IMPCommonSignature(implicit val env: Environment with importINT) {
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

  val div = SimpleFreeLabel2("_/_")
  val plus = SimpleFreeLabel2("_+_")
  val leq = SimpleFreeLabel2("_<=_")
  val not = SimpleFreeLabel1("!_")
  val and = SimpleFreeLabel2("_&&_")
  val emptyBlock = SimpleFreeLabel0("{}")
  val block = SimpleFreeLabel1("{_}")
  val assign = SimpleFreeLabel2("_:=_")
  val ifthenelse = SimpleFreeLabel3("if_then_else_")
  val whiledo = SimpleFreeLabel2("while(_)_")
  val seq = SimpleFreeLabel2("__")
  val program = SimpleFreeLabel2("_;_")

  val T = SimpleFreeLabel2("<T>")
  val k = SimpleFreeLabel1("<k>")
  val state = SimpleFreeLabel1("<state>")

  val varBinding = SimpleFreeLabel2("_->_")

  val emptyIntList = SimpleFreeLabel0(".List{Int}")

  val emptyStates = SimpleFreeLabel0("emptyStates")

  val statesMap = MapLabel("_StatesMap_", {
    case varBinding(variable, _) => variable
  }, emptyStates())

  val emptyk = SimpleFreeLabel0(".K")
}
