package org.kframework.kale

import org.kframework.kale.standard.MightBeSolved
import org.kframework.kore
import org.kframework.kore.implementation.DefaultBuilders

import scala.collection.Seq

trait MatchingLogic {
  // Constants
  val Bottom: Truth with kore.Bottom
  val Top: Truth with Substitution with kore.Top

  // Labels
  val Variable: VariableLabel
  val And: AndLabel
  val Or: OrLabel
  val Rewrite: RewriteLabel
  val Equality: EqualityLabel
  val Truth: TruthLabel
  val Not: NotLabel
  val Next: NextLabel
  val Exists: ExistsLabel
}

trait DomainValueLabel[T] extends LeafLabel[T] {

  def apply(v: T): DomainValue[T]
}

trait DomainValue[T] extends Leaf[T] with kore.DomainValue {
  val label: DomainValueLabel[T]

  val isGround = true

  override lazy val isPredicate: Boolean = false

  override def toString: String = data.toString

  override def symbol = label

  override def value = DefaultBuilders.Value(data.toString)
}

trait Sort extends kore.Sort {
  val name: String

  def smtName: String = name

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

trait Variable extends Leaf[(Name, Sort)] with kore.Variable {
  val label: VariableLabel
  val name: Name
  val sort: Sort
  lazy val data = (name, sort)
  val isGround = false
  override lazy val isPredicate: Boolean = false

  override def toString: String = name.str

  override def canEqual(o: Any): Boolean = o.isInstanceOf[Variable]

  override def equals(o: Any): Boolean = o match {
    case v: Variable => v.name == this.name
    case _ => false
  }

  override val variables: Set[Variable] = Set(this)
}

trait TruthLabel extends LeafLabel[Boolean] {
  override protected[this] def internalInterpret(s: String): Boolean = s.toBoolean
}

trait Truth extends Leaf[Boolean] {
  val isGround = true
  override lazy val isPredicate: Boolean = true
}

trait Top extends Truth with Substitution with kore.Top {
  override val boundVariables: Set[Variable] = Set()
}

trait Bottom extends Truth with kore.Bottom


trait AndLabel extends AssocCommWithIdLabel with Z3Builtin {

  import env._

  override val identity = Top
  assert(identity != null)

  def asSubstitutionAndTerms(t: Term): (Substitution, Set[Term])

  def combine(label: Node)(tasks: MightBeSolved*): Term

  def combine(label: NodeLabel)(tasks: MightBeSolved*): Term
}

trait OrLabel extends AssocCommWithIdLabel with Z3Builtin {
  override val identity = env.Bottom
  assert(identity != null)
}

trait RewriteLabel extends Label2

trait EqualityLabel extends Label2 with Z3Builtin {
  def binding(_1: Variable, _2: Term): Binding
}

trait NotLabel extends Label1 with Z3Builtin

trait ExistsLabel extends Label2

trait Exists extends kore.Exists

trait Equals extends kore.Equals with Node2 with BinaryInfix {
  override lazy val isPredicate: Boolean = true
}

trait Binding extends Equals with Substitution {
  override val boundVariables: Set[Variable] = Set(_1.asInstanceOf[Variable])

  override def remove(v: Variable): Substitution = if (_1 == v) env.Top else this
}

trait And extends kore.And with AssocComm {
  self: And =>
  val predicates: Term
  val nonPredicates: Option[Term]

  override lazy val isPredicate: Boolean = nonPredicates.isEmpty
}

trait Or extends kore.Or with AssocComm {
  val label: OrLabel

  def map(f: Term => Term): Term = label.map(f)(this)
}

trait Rewrite extends kore.Rewrite with Node2 with BinaryInfix {
  override lazy val isPredicate: Boolean = ???
}

trait Application extends Node with kore.Application {

  override lazy val isPredicate: Boolean = false

  // for KORE
  override def symbol: kore.Symbol = label

  override def args: Seq[kore.Pattern] = children.toSeq
}

trait NextLabel extends Label1
