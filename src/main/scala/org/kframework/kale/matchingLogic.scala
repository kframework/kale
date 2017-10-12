package org.kframework.kale

import org.kframework.kale.standard.{MightBeSolved, Sort}
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.{kale, kore}

import scala.collection.Seq

trait MatchingLogicMixin extends Mixin {
  env: Environment =>

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
  val ForAll: ForAllLabel

  def sort(l: Label, children: Seq[Term]): Sort

  def sort(l: Label): kale.Sort

  def isSort(sort: kore.Sort, term: Term): Boolean
}

trait DomainValueLabel[T] extends LeafLabel[T] with ThisRoaring {

  override val isPredicate: Option[Boolean] = Some(false)

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
  def apply(name: String): Variable = apply((standard.Name(name), Sort.K))

  def apply(name: String, sort: kale.Sort): Variable = apply((standard.Name(name), sort))

  def apply(v: (Name, Sort)): Variable

  def apply(name: kale.Name): Variable = apply((name, Sort.K))
}

trait Name extends kore.Name {
  override def toString = str
}

trait Variable extends Leaf[(Name, Sort)] with kore.SortedVariable {
  val label: VariableLabel
  val name: Name
  val sort: Sort
  lazy val data = (name, sort)
  val isGround = false
  override lazy val isPredicate: Boolean = false

  override def toString: String = name.str + (
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

trait Top extends Truth with Substitution with kore.Top {
  override val boundVariables: Set[Variable] = Set()
}

trait Bottom extends Truth with kore.Bottom


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

trait Exists extends kore.Exists {
  val v: Variable
  val p: Term
}

trait ForAllLabel extends Label2

trait ForAll extends Node2 with kore.ForAll

trait Equals extends kore.Equals with Node2 with BinaryInfix {
  override lazy val isPredicate: Boolean = true
}

trait Binding extends Equals with Substitution with NotRoaringTerm {
  override val boundVariables: Set[Variable] = Set(_1.asInstanceOf[Variable])

  override def filter(f: Variable => Boolean): Substitution = if (f(_1.asInstanceOf[Variable])) this else env.Top
}

trait And extends kore.And with AssocComm {
  self: And =>
  val predicate: Term
  val nonPredicate: Term

  override lazy val isPredicate: Boolean = nonPredicate == label.env.Top
}

trait Or extends kore.Or with AssocComm {
  val label: OrLabel

  def map(f: Term => Term): Term = label.map(f)(this)
}

trait Rewrite extends kore.Rewrite with Node2 with BinaryInfix {
  override lazy val isPredicate: Boolean = false
}

trait Application extends Node with kore.Application {

  // for KORE
  override def symbol: kore.Symbol = label

  override def args: Seq[kore.Pattern] = children.toSeq
}

trait NextLabel extends Label1
