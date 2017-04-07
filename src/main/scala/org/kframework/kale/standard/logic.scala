package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.util.{NameFromObject, Named}
import org.kframework.minikore.interfaces.pattern

import scala.collection.Map

case class SimpleConstant[T](label: ConstantLabel[T], value: T) extends Constant[T]

case class SimpleVariableLabel(implicit val env: Environment) extends Named("#Variable") with VariableLabel {
  def apply(name: String): Variable = apply(name, Sort.K)
  def apply(nameAndSort: (String, kale.Sort)): Variable = SimpleVariable(nameAndSort._1, nameAndSort._2)
}

case class SimpleVariable(name: String, sort: kale.Sort)(implicit env: Environment) extends Variable with pattern.Variable {
  val label = env.Variable

  // FOR KORE
  override def build(_1: pattern.Name, _2: pattern.Sort): SimpleVariable = {
    assert(_2.str == "K")
    SimpleVariable(_1, _2.asInstanceOf[kale.Sort])
  }

  override def _1: pattern.Name = name

  override def _2: pattern.Sort = pattern.Sort("K")
}

case class SimpleTruthLabel(implicit val env: Environment) extends NameFromObject with TruthLabel {
  def apply(v: Boolean) = if (v) env.Top else env.Bottom
}

abstract class Truth(val value: Boolean)(implicit val env: Environment) extends kale.Truth {
  val label = env.Truth
}

case class TopInstance(implicit eenv: Environment) extends Truth(true) with kale.Top {
  override def get(v: Variable): Option[Term] = None

  def asMap = Map()

  override def toString: String =  "⊤"

  def apply(t: Term): Term = t

  // FOR KORE
  override def build(): pattern.Top = this
}

case class BottomInstance(implicit eenv: Environment) extends Truth(false) with kale.Bottom {
  override def toString: String =  "⊥"

  // FOR KORE
  override def build(): pattern.Bottom = this
}