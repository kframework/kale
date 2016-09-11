package org.kframework.kale

import org.kframework.kale.context.AnywhereContextLabel

import scala.collection._
import scala.language.implicitConversions

case class Environment() {
  val uniqueLabels = mutable.Map[String, Label]()

  def labels = uniqueLabels.values.toSet

  def register(label: Label): Int = {
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
  val Substitution = SubstitutionLabel()
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
    if(terms.contains(Bottom))
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

trait ConstantLabel[T] extends NameFromObject with LeafLabel[T] {
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

trait Hooked {
  def f(t: Term): Term
}

case class TruthLabel(implicit val env: Environment) extends LeafLabel[Boolean] with NameFromObject {
  def apply(v: Boolean) = if (v) env.Top else env.Bottom
}

class Truth(val value: Boolean)(implicit val env: Environment) extends Leaf[Boolean] {
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
