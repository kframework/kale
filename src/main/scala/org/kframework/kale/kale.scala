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

trait Term extends Iterable[Term] with pattern.Pattern {
  def updateAt(i: Int)(t: Term): Term

  val label: Label

  val isGround: Boolean

  private var att: Any = null

  def setHiddenAttDONOTUSE(att: Any) = this.att = att

  def getHiddenAttDONOTUSE = this.att

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

  def updateAt(i: Int)(t: Term): Term = if (i <= 0 || i > productArity) {
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

  override def toString = label + "(" + value.toString + ")"
}

trait NameFromObject {
  val name = this.getClass.getName.drop(5)
}

trait ConstantLabel[T] extends LeafLabel[T] {

  def apply(v: T) = Constant(this, v)

  def interpret(s: String): Constant[T] = this(internalInterpret(s))

  // FOR KORE
  // remove this and all descendants if getting rid of Constant.build
  protected[this] def internalInterpret(s: String): T
  // FOR KORE
}

case class Constant[T](label: ConstantLabel[T], value: T) extends Leaf[T] with pattern.DomainValue {
  // FOR KORE
  def build(symbol: pattern.Symbol, content: pattern.Value): pattern.DomainValue = {
    symbol.asInstanceOf[ConstantLabel[_]].interpret(content)
  }
  def _1 = label
  def _2 = value.toString
  // /FOR KORE

  val isGround = true

  override def toString = value.toString
}

trait VariableLabel extends LeafLabel[String]

case class SimpleVariableLabel(implicit val env: Environment) extends NameFromObject with VariableLabel {
  def apply(name: String): Variable = SimpleVariable(name)
}

trait Variable extends Leaf[String] {
  val label: VariableLabel
  val name: String
  lazy val value = name
  val isGround = false

  override def toString = name
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

  override def toString = "⊤"

  def apply(t: Term) = t

  // FOR KORE
  override def build(): pattern.Top = this
}

private case class BottomInstance(implicit eenv: Environment) extends Truth(false) with pattern.Bottom {
  override def toString = "⊥"

  // FOR KORE
  override def build(): pattern.Bottom = this
}

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

object PrimitiveFunction2 {
  def apply[A](name: String, aLabel: LeafLabel[A], f: (A, A) => A)(implicit env: Environment): PrimitiveFunction2[A, A, A] =
    PrimitiveFunction2(name, aLabel, aLabel, aLabel, f)
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

trait FunctionDefinedByRewriting extends FunctionLabel {
  val env: CurrentEnvironment
  private var p_rewriter: Option[Rewriter] = None

  def rewriter: Rewriter = p_rewriter.get

  //throw new AssertionError("Set rules before sealing the environment. Or at least before trying to create new terms in the sealed environment.")

  def setRules(rules: Set[Rewrite]): Unit = {
    p_rewriter = Some(Rewriter(SubstitutionApply(env), Matcher(env).default, env)(rules))
  }

  def tryToApply(res: Term): Option[Term] =
    if (env.isSealed && rewriter.rules.nonEmpty) {
      // do not try to execute the function before the env is sealed as it would trigger the lazy initialization fo the Rewriter,
      // and a Rewriter can only be built once the Environment is sealed
      val Bottom = rewriter.env.Bottom
      rewriter.step(res).find(t => t.label != env.And && t.label != env.Or)
    } else {
      None
    }
}

case class FunctionDefinedByRewritingLabel0(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with FunctionalLabel0 {
  def f(): Option[Term] = tryToApply(FreeNode0(this))
}

case class FunctionDefinedByRewritingLabel1(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel1 {
  def f(_1: Term): Option[Term] = tryToApply(FreeNode1(this, _1))
}

case class FunctionDefinedByRewritingLabel2(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel2 {
  def f(_1: Term, _2: Term): Option[Term] = tryToApply(FreeNode2(this, _1, _2))
}

case class FunctionDefinedByRewritingLabel3(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel3 {
  def f(_1: Term, _2: Term, _3: Term): Option[Term] = tryToApply(FreeNode3(this, _1, _2, _3))
}

case class FunctionDefinedByRewritingLabel4(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel4 {
  def f(_1: Term, _2: Term, _3: Term, _4: Term): Option[Term] = tryToApply(FreeNode4(this, _1, _2, _3, _4))
}
