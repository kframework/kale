package org.kframework.kale

import org.kframework.kale.context.AnywhereContextLabel

import scala.collection._
import scala.language.implicitConversions

case class Environment() {
  val uniqueLabels = mutable.Map[String, Label]()

  def labels = uniqueLabels.values.toSet

  private var pisSealed = false

  def seal(): Unit = pisSealed = true

  def isSealed = pisSealed

  def register(label: Label): Int = {
    assert(!isSealed, "The environment is sealed")
    assert(label != null)

    if (uniqueLabels.contains(label.name))
      throw new AssertionError("Label " + label.name + " already registered. The current env is: \n" + this)

    uniqueLabels.put(label.name, label)
    uniqueLabels.size
  }

  def label(labelName: String): Label = uniqueLabels(labelName)

  override def toString = {
    "nextId: " + uniqueLabels.size + "\n" + uniqueLabels.mkString("\n")
  }

  implicit private val tthis = this

  val Variable = VariableLabel()

  val Truth = TruthLabel()

  val Top: Truth with Substitution = TopInstance()
  val Bottom: Truth = BottomInstance()

  val Equality = EqualityLabel()
  val And = AndLabel()
  val Or = OrLabel()
  val Rewrite = RewriteLabel()

  val AnywhereContext = AnywhereContextLabel()

  val builtin = new Builtins()(this)

  def bottomize(_1: Term)(f: => Term): Term = {
    if (Bottom == _1)
      Bottom
    else
      f
  }

  def bottomize(_1: Term, _2: Term)(f: => Term): Term = {
    if (Bottom == _1 || Bottom == _2)
      Bottom
    else
      f
  }

  def bottomize(terms: Term*)(f: => Term): Term = {
    if (terms.contains(Bottom))
      Bottom
    else
      f
  }

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(v.name + Math.random().toInt)))).toMap)
    rename(t).asInstanceOf[T]
  }
}

trait Label extends MemoizedHashCode {
  val env: Environment

  val name: String

  val id: Int = env.register(this)

  override def equals(other: Any) = other match {
    case that: Label => this.name == that.name
    case _ => false
  }

  override def computeHashCode = name.hashCode

  override def toString = name
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

trait Term extends Iterable[Term] {
  def updateAt(i: Int)(t: Term): Term

  val label: Label

  val isGround: Boolean

  private var att: Any = null

  def setHiddenAttDONOTUSE(att: Any) = this.att = att

  def getHiddenAttDONOTUSE = this.att

  def iterator(): Iterator[Term]

  override def hashCode = label.hashCode
}

object Node {
  def unapply(t: Term): Option[(NodeLabel, Iterator[Term])] = t match {
    case t: Node => Some(t.label, t.iterator)
    case _ => None
  }
}

trait Node extends Term with Product {
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
}

case class Constant[T](label: ConstantLabel[T], value: T) extends Leaf[T] {
  val isGround = true

  override def toString = value.toString
}

case class VariableLabel(implicit val env: Environment) extends NameFromObject with LeafLabel[String] {
  def apply(name: String): Variable = SimpleVariable(name)
}

trait Variable extends Leaf[String] {
  val label: VariableLabel
  val name: String
  lazy val value = name
  val isGround = false

  override def toString = name
}

case class SimpleVariable(name: String)(implicit env: Environment) extends Variable {
  val label = env.Variable
}

trait FormulaLabel

case class TruthLabel(implicit val env: Environment) extends LeafLabel[Boolean] with NameFromObject with FormulaLabel {
  def apply(v: Boolean) = if (v) env.Top else env.Bottom
}

class Truth(val value: Boolean)(implicit val env: Environment) extends Leaf[Boolean] {
  val label = env.Truth
  val isGround = true
}

private case class TopInstance(implicit eenv: Environment) extends Truth(true) with Substitution {
  override def get(v: Variable): Option[Term] = None

  override def toString = "⊤"

  def apply(t: Term) = t
}

private case class BottomInstance(implicit eenv: Environment) extends Truth(false) {
  override def toString = "⊥"
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

object PrimitiveFunction2 {
  def apply[A, R](name: String, aLabel: LeafLabel[A], rLabel: LeafLabel[R], f: (A, A) => R)(implicit env: Environment): PrimitiveFunction2[A, A, R] =
    PrimitiveFunction2(name, aLabel, aLabel, rLabel, f)

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
  val env: Environment
  private var p_rewriter: Option[Rewriter] = None

  def rewriter: Rewriter = p_rewriter.get

  //throw new AssertionError("Set rules before sealing the environment. Or at least before trying to create new terms in the sealed environment.")

  def setRules(rules: Set[Rewrite]): Unit = {
    p_rewriter = Some(Rewriter(SubstitutionApply(env), Matcher(env), env)(rules))
  }

  def tryToApply(res: Term): Option[Term] = if (env.isSealed && rewriter.rules.nonEmpty) {
    // do not try to execute the function before the env is sealed as it would trigger the lazy initialization fo the Rewriter,
    // and a Rewriter can only be built once the Environment is sealed
    val Bottom = rewriter.env.Bottom
    val ress = rewriter.executionStep(res)
    ress match {
      case Bottom => None
      case t if t.label != env.And && t.label != env.Or => Some(t)
      case _ => None
    }
  } else {
    None
  }
}

case class FunctionDefinedByRewritingLabel0(name: String)(implicit val env: Environment) extends FunctionDefinedByRewriting with FunctionalLabel0 {
  def f(): Option[Term] = tryToApply(FreeNode0(this))
}

case class FunctionDefinedByRewritingLabel1(name: String)(implicit val env: Environment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel1 {
  def f(_1: Term): Option[Term] = tryToApply(FreeNode1(this, _1))
}

case class FunctionDefinedByRewritingLabel2(name: String)(implicit val env: Environment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel2 {
  def f(_1: Term, _2: Term): Option[Term] = tryToApply(FreeNode2(this, _1, _2))
}

case class FunctionDefinedByRewritingLabel3(name: String)(implicit val env: Environment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel3 {
  def f(_1: Term, _2: Term, _3: Term): Option[Term] = tryToApply(FreeNode3(this, _1, _2, _3))
}

case class FunctionDefinedByRewritingLabel4(name: String)(implicit val env: Environment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel4 {
  def f(_1: Term, _2: Term, _3: Term, _4: Term): Option[Term] = tryToApply(FreeNode4(this, _1, _2, _3, _4))
}
