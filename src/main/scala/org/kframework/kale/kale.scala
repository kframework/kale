package org.kframework.kale

import scala.collection._
import scala.language.implicitConversions

object UniqueId {
  var nextId = 0

  def apply(): Int = {
    nextId += 1
    nextId - 1
  }
}

trait UniqueId {
  val id = UniqueId()
}

trait Label extends MemoizedHashCode {
  val name: String
  val id: Int

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
  val name = this.getClass.getName.drop(5).dropRight(1)
}

trait ConstantLabel[T] extends LeafLabel[T] with NameFromObject with UniqueId {
  def apply(v: T) = Constant(this, v)
}

case class Constant[T](label: ConstantLabel[T], value: T) extends Leaf[T] {
  val isGround = true
  override def toString = value.toString
}

object Variable extends LeafLabel[String] with NameFromObject with UniqueId {
  override def apply(name: String): Variable = SimpleVariable(name)
}

trait Variable extends Leaf[String] {
  val label = Variable
  val name: String
  lazy val value = name
  val isGround = false
}

case class SimpleVariable(name: String) extends Variable

trait Hooked {
  def f(t: Term): Term
}

object Truth extends LeafLabel[Boolean] with NameFromObject with UniqueId {
  def apply(v: Boolean) = if (v) Top else Bottom
}

class Truth(val value: Boolean) extends Leaf[Boolean] {
  val label = Truth
  val isGround = true
}

object Top extends Truth(true) with Substitution {
  override def get(v: Variable): Option[Term] = None

  override def toString = "⊤"
}

object Bottom extends Truth(false) {
  override def toString = "⊥"
}




