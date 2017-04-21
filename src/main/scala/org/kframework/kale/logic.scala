package org.kframework.kale

import org.kframework.kore

trait PredicateLabel

trait DomainValueLabel[T] extends LeafLabel[T] {

  def apply(v: T): DomainValue[T]

  // FOR KORE

  def interpret(v: Value[T]): DomainValue[T] = this(v.data)
}

// for KORE
case class Value[T](data: T) extends kore.Value {
  override def str: String = data.toString
}

trait DomainValue[T] extends Leaf[T] with kore.DomainValue {
  val label: DomainValueLabel[T]

  val isGround = true

  override def toString: String = data.toString

  override def symbol = label

  override def value = Value(data)
}

trait Sort extends kore.Sort {
  val name: String

  override def equals(other: Any): Boolean = other match {
    case that: Sort => that.name == name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode

  // FOR KORE
  override val str = name
}

trait VariableLabel extends LeafLabel[(Name, Sort)] {
  def apply(v: (Name, Sort)): Variable
}

trait Name extends kore.Name {
  override def toString = str
}

trait Variable extends Leaf[(Name, Sort)] {
  val label: VariableLabel
  val name: Name
  val sort: Sort
  lazy val data = (name, sort)
  val isGround = false

  override def toString: String = name.str

  override def canEqual(o: Any): Boolean = o.isInstanceOf[Variable]

  override def equals(o: Any): Boolean = o match {
    case v: Variable => v.name == this.name
    case _ => false
  }
}

trait TruthLabel extends LeafLabel[Boolean] with PredicateLabel

trait Truth extends Leaf[Boolean] {
  val isGround = true
}

trait Top extends Truth with Substitution with kore.Top

trait Bottom extends Truth with kore.Bottom


trait AndLabel extends AssocCommWithIdLabel {
  override val identity = env.Top
  assert(identity != null)
}

trait OrLabel extends AssocCommWithIdLabel {
  override val identity = env.Bottom
  assert(identity != null)
}

trait RewriteLabel extends Label2

trait EqualityLabel extends Label2 with PredicateLabel {
  def binding(_1: Variable, _2: Term): Binding
}

trait NotLabel extends Label1

trait Equals extends kore.Equals with Node2 with BinaryInfix

trait Binding extends Equals with Substitution

trait And extends kore.And with AssocComm {
  self: And =>
  val formulas: Term
  val nonFormula: Option[Term]
}

trait Or extends kore.Or with AssocComm

trait Rewrite extends kore.Rewrite with Node2 with BinaryInfix

trait Application extends Node with kore.Application {

  // for KORE
  override def symbol: kore.Symbol = label

  override def args: Seq[kore.Pattern] = children.toSeq
}
