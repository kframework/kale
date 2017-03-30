package org.kframework.kale

import scala.collection._
import scala.language.implicitConversions
import org.kframework.minikore.interfaces.{pattern, tree}

trait Label extends MemoizedHashCode with pattern.Symbol {
  val env: Environment

  val name: String

  val id: Int = env.register(this)

  override def equals(other: Any) = other match {
    case that: Label => this.name == that.name
    case _ => false
  }

  override def computeHashCode = name.hashCode

  override def toString = name

  // FOR KORE
  def str: String = name
}

trait NodeLabel extends Label {
  def unapplySeq(t: Term): Option[Seq[Term]] = t match {
    case t: Node if t.label == this => Some(t.iterator.toSeq)
    case _ => None
  }

  val arity: Int

  def apply(l: Iterable[Term]): Term = if (l.size == arity) {
    constructFromChildren(l)
  } else {
    throw new AssertionError("Incorrect number of children for constructing a " + name + ". Expected: " + arity + " but found: " + l.size)
  }

  protected def constructFromChildren(l: Iterable[Term]): Term
}

trait LeafLabel[T] extends Label {
  def apply(t: T): Term

  def unapply(t: Term): Option[T] = t match {
    case t: Leaf[T] if t.label == this => Some(t.value)
    case _ => None
  }
}

trait Term extends Iterable[Term] with pattern.Pattern with HasAtt {
  def updateAt(i: Int)(t: Term): Term

  val label: Label

  val isGround: Boolean

  def iterator(): Iterator[Term]

  // TODO: better implementation
  override def hashCode = this.label.hashCode
}

object Node {
  def unapply(t: Term): Option[(NodeLabel, Iterator[Term])] = t match {
    case t: Node => Some(t.label, t.iterator)
    case _ => None
  }
}

trait Node extends Term with Product with tree.Node {
  // FOR KORE
  override def args: Seq[pattern.Pattern] = iterator.toSeq

  override def build(children: Seq[pattern.Pattern]): pattern.Pattern = {
    // downcasting to Term, but it will fail somewhere in Label
    label(children.asInstanceOf[Seq[Term]])
  }

  // /FOR KORE

  val label: NodeLabel

  def updateAt(i: Int)(t: Term): Term = if (i < 0 || i >= productArity) {
    throw new IndexOutOfBoundsException(label + " has " + productArity + " children. Trying to update index _" + i)
  } else {
    innerUpdateAt(i, t)
  }

  protected def innerUpdateAt(i: Int, t: Term): Term

  def iterator: Iterator[Term]

  override def toString = label + "(" + iterator.mkString(", ") + ")"
}

trait Leaf[T] extends Term {
  def iterator = Iterator.empty

  def updateAt(i: Int)(t: Term): Term = throw new IndexOutOfBoundsException("Leaves have no children. Trying to update index _" + i)

  val label: LeafLabel[T]
  val value: T

  override def toString: String = label + "(" + value.toString + ")"

  def copy(): Term = label(value).setAtts(attributes)
}

trait NameFromObject {
  val name: String = this.getClass.getName.drop(5)
}

trait ConstantLabel[T] extends LeafLabel[T] {

  def apply(v: T) = new Constant(this, v)

  def interpret(s: String): Constant[T] = this (internalInterpret(s))

  // FOR KORE
  // remove this and all descendants if getting rid of Constant.build
  protected[this] def internalInterpret(s: String): T

  // FOR KORE
}

class Constant[T](val label: ConstantLabel[T], val value: T) extends Leaf[T] with pattern.DomainValue {
  // FOR KORE
  def build(symbol: pattern.Symbol, content: pattern.Value): pattern.DomainValue = {
    symbol.asInstanceOf[ConstantLabel[_]].interpret(content)
  }

  def _1 = label

  def _2 = value.toString

  // /FOR KORE

  val isGround = true

  override def toString: String = value.toString
}

trait VariableLabel extends LeafLabel[String]

case class SimpleVariableLabel(implicit val env: Environment) extends NameFromObject with VariableLabel {
  def apply(name: String): Variable = SimpleVariable(name)
}

trait Variable extends Leaf[String] {
  val label: VariableLabel
  val name: String
  lazy val value: String = name
  val isGround = false

  override def toString: String = name
}

case class SimpleVariable(name: String)(implicit env: Environment) extends Variable with pattern.Variable {
  val label = env.Variable

  // FOR KORE
  override def build(_1: pattern.Name, _2: pattern.Sort): SimpleVariable = {
    assert(_2.str == "K")
    SimpleVariable(_1)
  }

  override def _1: pattern.Name = name

  override def _2: pattern.Sort = pattern.Sort("K")

  // FOR KORE
}

trait FormulaLabel

trait TruthLabel extends LeafLabel[Boolean] with FormulaLabel

case class SimpleTruthLabel(implicit val env: Environment) extends NameFromObject with TruthLabel {
  def apply(v: Boolean) = if (v) env.Top else env.Bottom
}

class Truth(val value: Boolean)(implicit val env: Environment) extends Leaf[Boolean] {
  val label = env.Truth
  val isGround = true
}

private case class TopInstance(implicit eenv: Environment) extends Truth(true) with Substitution with pattern.Top {
  override def get(v: Variable): Option[Term] = None

  def asMap = Map()

  override def toString = "⊤"

  def apply(t: Term): Term = t

  // FOR KORE
  override def build(): pattern.Top = this
}

private case class BottomInstance(implicit eenv: Environment) extends Truth(false) with pattern.Bottom {
  override def toString = "⊥"

  // FOR KORE
  override def build(): pattern.Bottom = this
}

// Defining functions

trait FunctionLabel {
  val name: String
}

trait FunctionalLabel0 extends Label0 with FunctionLabel {
  def f(): Option[Term]

  def apply(): Term = f() getOrElse FreeNode0(this)
}

trait PurelyFunctionalLabel1 extends Label1 with FunctionLabel {
  def f(_1: Term): Option[Term]

  def apply(_1: Term): Term = f(_1) getOrElse FreeNode1(this, _1)
}

trait PurelyFunctionalLabel2 extends Label2 with FunctionLabel {
  def f(_1: Term, _2: Term): Option[Term]

  def apply(_1: Term, _2: Term): Term = f(_1, _2) getOrElse FreeNode2(this, _1, _2)
}

object Operator {
  def apply[A, R](name: String, aLabel: LeafLabel[A], rLabel: LeafLabel[R], f: (A, A) => R)(implicit env: Environment): PrimitiveFunction2[A, A, R] =
    PrimitiveFunction2(name, aLabel, aLabel, rLabel, f)
}

object PrimitiveFunction1 {
  def apply[A](name: String, aLabel: LeafLabel[A], f: A => A)(implicit env: Environment): PrimitiveFunction1[A, A] =
    PrimitiveFunction1(name, aLabel, aLabel, f)
}

case class PrimitiveFunction1[A, R](name: String, aLabel: LeafLabel[A], rLabel: LeafLabel[R], primitiveF: A => R)(implicit val env: Environment) extends PurelyFunctionalLabel1 {
  def f(_1: Term): Option[Term] = _1 match {
    case aLabel(a) => Some(rLabel(primitiveF(a)))
    case _ => None
  }
}

object PrimitiveFunction2 {
  def apply[A](name: String, aLabel: LeafLabel[A], f: (A, A) => A)(implicit env: Environment): PrimitiveFunction2[A, A, A] =
    PrimitiveFunction2(name, aLabel, aLabel, aLabel, f)
}

case class PrimitiveFunction2[A, B, R](name: String, aLabel: LeafLabel[A], bLabel: LeafLabel[B], rLabel: LeafLabel[R], primitiveF: (A, B) => R)(implicit val env: Environment) extends PurelyFunctionalLabel2 {
  def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
    case (aLabel(a), bLabel(b)) => Some(rLabel(primitiveF(a, b)))
    case _ => None
  }
}

trait PurelyFunctionalLabel3 extends Label3 with FunctionLabel {
  def f(_1: Term, _2: Term, _3: Term): Option[Term]

  def apply(_1: Term, _2: Term, _3: Term): Term = f(_1, _2, _3) getOrElse FreeNode3(this, _1, _2, _3)
}

trait PurelyFunctionalLabel4 extends Label4 with FunctionLabel {
  def f(_1: Term, _2: Term, _3: Term, _4: Term): Option[Term]

  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term = f(_1, _2, _3, _4) getOrElse FreeNode4(this, _1, _2, _3, _4)
}

// Fixed-arity

trait Label0 extends Function0[Term] with NodeLabel {
  val arity = 0

  def apply(): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply()
}
  def setRules(rules: Set[Rewrite]): Unit = {
    p_rewriter = Some(Rewriter(SubstitutionApply(env), new Matcher(env).applier, env)(rules))
  }

trait Label1 extends (Term => Term) with NodeLabel {
  val arity = 1

  def apply(_1: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head)
}

trait Label2 extends ((Term, Term) => Term) with NodeLabel {
  val arity = 2

  def apply(_1: Term, _2: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head)

  def unapply(t: Term): Option[(Term, Term)] = t match {
    case n: Node2 if n.label == this => Some(n._1, n._2)
    case _ => None
  }
}

trait Label3 extends NodeLabel {
  val arity = 3

  def apply(_1: Term, _2: Term, _3: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head)
}

trait Label4 extends NodeLabel {
  val arity = 4

  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head)
}

trait Label5 extends NodeLabel {
  val arity = 5

  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head, l.tail.tail.tail.tail.head)
}

trait Label6 extends NodeLabel {
  val arity = 6

  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head, l.tail.tail.tail.tail.head, l.tail.tail.tail.tail.tail.head)
}

trait Node0 extends Node {
  val label: Label0

  val isGround = true

  def innerUpdateAt(i: Int, t: Term): Term = throw new AssertionError("unreachable code")

  def iterator = Iterator.empty
}

trait Node1 extends Node with Product1[Term] {
  val label: Label1

  val isGround = _1.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t)
  }

  def iterator = Iterator(_1)

  // FOR KORE
  def build(_1: pattern.Pattern): pattern.Pattern = label.asInstanceOf[Label2](_1.asInstanceOf[Term])
}

trait Node2 extends Node with Product2[Term, Term] {
  val label: Label2

  lazy val isGround = _1.isGround && _2.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2)
    case 2 => label(_1, t)
  }

  def iterator = Iterator(_1, _2)

  // FOR KORE
  def build(_1: pattern.Pattern, _2: pattern.Pattern): pattern.Pattern = label.asInstanceOf[Label2](_1.asInstanceOf[Term], _2.asInstanceOf[Term])
}

trait Node3 extends Node with Product3[Term, Term, Term] {
  val label: Label3

  val isGround = _1.isGround && _2.isGround && _3.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3)
    case 2 => label(_1, t, _3)
    case 3 => label(_1, _2, t)
  }

  def iterator = Iterator(_1, _2, _3)
}

trait Node4 extends Node with Product4[Term, Term, Term, Term] {
  val label: Label4

  val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3, _4)
    case 2 => label(_1, t, _3, _4)
    case 3 => label(_1, _2, t, _4)
    case 4 => label(_1, _2, _3, t)
  }

  def iterator = Iterator(_1, _2, _3, _4)
}

trait Node5 extends Node with Product5[Term, Term, Term, Term, Term] {
  val label: Label5

  val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3, _4, _5)
    case 2 => label(_1, t, _3, _4, _5)
    case 3 => label(_1, _2, t, _4, _5)
    case 4 => label(_1, _2, _3, t, _5)
    case 5 => label(_1, _2, _3, _4, t)
  }

  def iterator = Iterator(_1, _2, _3, _4, _5)
}

trait Node6 extends Node with Product6[Term, Term, Term, Term, Term, Term] {
  val label: Label6

  val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround && _6.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3, _4, _5, _6)
    case 2 => label(_1, t, _3, _4, _5, _6)
    case 3 => label(_1, _2, t, _4, _5, _6)
    case 4 => label(_1, _2, _3, t, _5, _6)
    case 5 => label(_1, _2, _3, _4, t, _6)
    case 6 => label(_1, _2, _3, _4, _5, t)
  }

  def iterator = Iterator(_1, _2, _3, _4, _5, _6)
}

// AC stuff

trait HasId {
  val identity: Term
}

trait AssocLabel extends Label2 {
  def apply(l: Iterable[Term]): Term

  val thisthis = this

  def asList(t: Term): Iterable[Term] = t.label match {
    case `thisthis` => t.asInstanceOf[Assoc].assocIterable
    case _ => List(t)
  }

  object iterable {
    def unapply(t: Term): Option[Iterable[Term]] = Some(asList(t))
  }

}

trait AssocWithIdLabel extends AssocLabel with HasId {

  // normalizing
  def apply(_1: Term, _2: Term) = {
    val l1 = asIterable(_1)
    val l2 = asIterable(_2)
    construct(l1 ++ l2)
  }

  def asIterable(t: Term): Iterable[Term] = t match {
    case `identity` => List[Term]()
    case x if x.label == this => x.asInstanceOf[Assoc].assocIterable
    case y => List(y)
  }

  // normalizing
  override def apply(list: Iterable[Term]): Term = list filterNot (_ == identity) match {
    case l if l.isEmpty => identity
    case l if l.size == 1 => l.head
    case l => (l fold identity) ((a, b) => apply(a, b))
  }

  def construct(l: Iterable[Term]): Term
}

trait AssocWithoutIdLabel extends AssocLabel {
  // todo
}

trait Assoc extends Node2 with BinaryInfix {
  override val label: AssocLabel
  val assocIterable: Iterable[Term]
}

trait Comm

trait AssocComm extends Assoc with Comm {
  def asSet: Set[Term]
}

trait CommLabel

trait AssocCommLabel extends AssocLabel with CommLabel {
  def asSet(t: Term): Set[Term]

  object set {
    def unapply(t: Term): Option[Set[Term]] = Some(asSet(t))
  }

}

// ML

trait AndLabel extends AssocCommLabel with FormulaLabel

trait OrLabel extends AssocCommLabel with FormulaLabel

trait RewriteLabel extends Label2

trait EqualityLabel extends Label2 with FormulaLabel

trait And extends Assoc with pattern.And {
  val formulas: Term
  val nonFormula: Option[Term]
}

trait Or extends AssocComm with pattern.Or

trait Rewrite extends Node2 with BinaryInfix with pattern.Rewrite

// Substitution

trait Substitution extends Term with (Term => Term) {
  def get(v: Variable): Option[Term]

  def apply(t: Term): Term
}

// Util

abstract class Named(val name: String)

trait BinaryInfix {
  self: Node2 =>
  override def toString = _1 + " " + label.name + " " + _2
}

// Free nodes

trait FreeLabel

case class FreeLabel0(name: String)(implicit val env: Environment) extends Label0 with FreeLabel {
  def apply(): Term = FreeNode0(this)
}

case class FreeLabel1(name: String)(implicit val env: Environment) extends Label1 with FreeLabel {
  def apply(_1: Term): Term = FreeNode1(this, _1)
}

case class FreeLabel2(name: String)(implicit val env: Environment) extends Label2 with FreeLabel {
  def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
}

case class FreeLabel3(name: String)(implicit val env: Environment) extends Label3 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term): Term = FreeNode3(this, _1, _2, _3)
}

case class FreeLabel4(name: String)(implicit val env: Environment) extends Label4 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term = FreeNode4(this, _1, _2, _3, _4)
}

case class FreeNode0(label: Label0) extends Node0

case class FreeNode1(label: Label1, _1: Term) extends Node1

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2

case class FreeNode3(label: Label3, _1: Term, _2: Term, _3: Term) extends Node3

case class FreeNode4(label: Label4, _1: Term, _2: Term, _3: Term, _4: Term) extends Node4

case class FreeNode5(label: Label5, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term) extends Node5

case class FreeNode6(label: Label6, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term) extends Node6
