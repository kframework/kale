package org.kframework.kale

import org.kframework.kale.standard.MightBeSolved
import org.kframework.kale

import scala.collection.Seq

trait MatchingLogicMixin extends Mixin {
  env: Environment =>

  // Constants
  val Bottom: Truth
  val Top: Truth with Substitution

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
  val ForAll: ForAllLabel

  def sort(l: Label, children: Seq[Term]): Sort

  def sort(l: Label): kale.Sort

  def isSort(sort: Sort, term: Term): Boolean
}

trait DomainValueLabel[T] extends LeafLabel[T] with ThisRoaring {

  override val isPredicate: Option[Boolean] = Some(false)

  def apply(v: T): DomainValue[T]
}

trait DomainValue[T] extends Leaf[T] {
  val label: DomainValueLabel[T]

  def isGround = true

  override lazy val isPredicate: Boolean = false

  override def toString: String = data.toString
}

trait Sort {
  val name: String

  def smtName: String = name

  override def equals(other: Any): Boolean = other match {
    case that: Sort => that.name == name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode
}

trait VariableLabel extends LeafLabel[(Name, Sort)] {
  def apply(name: String): Variable = apply((Name(name), standard.Sort.K))

  def apply(name: String, sort: kale.Sort): Variable = apply((Name(name), sort))

  def apply(v: (Name, Sort)): Variable

  def apply(name: kale.Name): Variable = apply((name, standard.Sort.K))
}

case class Name(name: String) {
  override def toString = name
}

trait Variable extends Leaf[(Name, Sort)] {
  val label: VariableLabel
  val name: Name
  val sort: Sort
  lazy val data = (name, sort)
  lazy val isGround = false
  override lazy val isPredicate: Boolean = false

  override def toString: String = name.toString + (
    if (sort.name == "K")
      ""
    else
      ":" + sort.name
    )

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
  override lazy val isPredicate: Boolean = false
}

trait Top extends Truth with Substitution {
  override val boundVariables: Set[Variable] = Set()
}

trait Bottom extends Truth


trait AndLabel extends CommutativeMonoid with Z3Builtin {

  import env._

  override val identity = Top
  assert(identity != null)

  def combine(label: Node)(tasks: MightBeSolved*): Term

  def combine(label: NodeLabel)(tasks: MightBeSolved*): Term

  override val isPredicate: Option[Boolean] = None
}

trait OrLabel extends CommutativeMonoid with Z3Builtin {
  override val identity = env.Bottom
  assert(identity != null)

  override val isPredicate: Option[Boolean] = None
}

trait RewriteLabel extends Label2 {
  override val isPredicate: Option[Boolean] = Some(false)
}

trait EqualityLabel extends Label2 with Z3Builtin with Predicate {
  def binding(_1: Variable, _2: Term): Binding
}

trait NotLabel extends Label1 with Z3Builtin

trait ExistsLabel extends Label2 {
  override val isPredicate: Option[Boolean] = None
}

trait Exists {
  val v: Variable
  val p: Term
}

trait ForAllLabel extends Label2

trait ForAll extends Node2

trait Equals extends Node2 with BinaryInfix {
  override lazy val isPredicate: Boolean = true
}

trait Binding extends Equals with Substitution with NotRoaringTerm {
  override val boundVariables: Set[Variable] = Set(_1.asInstanceOf[Variable])

  override def filter(f: Variable => Boolean): Substitution = if (f(_1.asInstanceOf[Variable])) this else env.Top
}

trait And extends AssocComm {
  self: And =>
  val predicate: Term
  val nonPredicate: Term

  override lazy val isPredicate: Boolean = nonPredicate == label.env.Top
}

trait Or extends AssocComm {
  val label: OrLabel

  def map(f: Term => Term): Term = label.map(f)(this)
}

trait Rewrite extends Node2 with BinaryInfix {
  override lazy val isPredicate: Boolean = false
}

trait NextLabel extends Label1
