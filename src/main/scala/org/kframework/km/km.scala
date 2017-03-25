package org.kframework.km

class Environment() extends kale.Environment {
  implicit private val env = this
  override val Variable = new VariableLabel()
}

sealed trait Sort
case class SortOf(name: String) extends Sort
case class SortBool() extends Sort
case class SortInt() extends Sort
case class SortReal() extends Sort
case class SortMap(key: Sort, value: Sort) extends Sort
case class SortList(elem: Sort) extends Sort

object SortK extends SortOf("K")
object SortMapK extends SortMap(SortK, SortK)
object SortListK extends SortList(SortK)

sealed trait Type extends Product2[Seq[Sort], Sort]

sealed trait Pattern
case class Application(symbol: kale.NodeLabel, children: Seq[Pattern]) extends Pattern
case class Variable(name: String, sort: Sort)(implicit val env: Environment) extends Pattern {
  val label = env.Variable
}

class VariableLabel(implicit override val env: Environment) extends kale.VariableLabel {
  def apply(name: String, sort: Sort): Variable = Variable(name, sort)
}

