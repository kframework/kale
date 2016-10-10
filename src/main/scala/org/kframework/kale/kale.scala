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

trait Formula

case class TruthLabel(implicit val env: Environment) extends LeafLabel[Boolean] with NameFromObject {
  def apply(v: Boolean) = if (v) env.Top else env.Bottom
}

class Truth(val value: Boolean)(implicit val env: Environment) extends Leaf[Boolean] with Formula {
  val label = env.Truth
  val isGround = true
}

private case class TopInstance(implicit eenv: Environment) extends Truth(true) with Substitution {
  override def get(v: Variable): Option[Term] = None

  override def toString = "⊤"
}

private case class BottomInstance(implicit eenv: Environment) extends Truth(false) {
  override def toString = "⊥"
}

trait FunctionLabel

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
  private var rules: Set[Rewrite] = _
  lazy val rewriter: Rewriter = {
    assert(rules != null, "Set rules before sealing the environment. Or at least before trying to create new terms in the sealed environment.")
    Rewriter(SubstitutionApply(env), Matcher(env), env)(rules)
  }

  def setRules(rules: Set[Rewrite]): Unit = this.rules = rules

  def tryToApply(res: Term): Option[Term] =
    if (env.isSealed) {
      // do not try to execute the function before the env is sealed as it would trigger the lazy initialization fo the Rewriter,
      // and a Rewriter can only be built once the Environment is sealed
      val Bottom = rewriter.env.Bottom
      val ress = rewriter.executionStep(res)
      ress match {
        case Bottom => None
        case t => Some(t)
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
