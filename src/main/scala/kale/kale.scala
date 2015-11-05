package kale

import scala.collection.generic.CanBuildFrom
import scala.collection.{IterableLike, SortedSet}
import scala.language.implicitConversions

trait Label extends MemoizedHashCode {
  val module: Module
  val name: String

  override def equals(other: Any) = other match {
    case that: Label => this.module == that.module && this.name == that.name
    case _ => false
  }
  override def computeHashCode = module.hashCode * 7 + name.hashCode

  override def toString = name + "@" + module
}

trait NodeLabel extends Label {
  def apply(children: Seq[Term]): Term
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

sealed trait Term extends Iterable[Term] {
  val label: Label
  def iterator(): Iterator[Term]
}
trait Node extends Term {
  val label: NodeLabel

  // basic implementation -- override for performance
  def map(f: (Term) => Term): Term
  // = label(iterator.map(f).toSeq)
  override def toString = label + "(" + mkString(", ") + ")"
}
trait Leaf[T] extends Term {
  def iterator = Iterator.empty
  val label: LeafLabel[T]
  val value: T
  def map(f: (Term) => Term): Term = this
  override def toString = label + "(" + value + ")"
}

trait Module {
  val name = this.getClass.getName.drop(5).dropRight(1)
  trait InModule {
    val module: Module = Module.this
  }
  override def toString() = name
}

trait NameFromObject {
  self: Label =>
  val name = this.getClass.getName.dropRight(1)
}

case class TokenLabel[T](module: Module, name: String) extends LeafLabel[T] {
  def apply(v: T) = Token(this, v)
}
case class Token[T](label: TokenLabel[T], value: T) extends Leaf[T]

trait Node0Label extends NodeLabel {
  final def apply(children: Seq[Term]): Term = {
    assert(children.size == 0)
    apply()
  }
  def apply(): Term
}
trait Node1Label extends NodeLabel {
  final def apply(children: Seq[Term]): Term = {
    assert(children.size == 1)
    apply(children(0))
  }
  def apply(_1: Term): Term
}
trait Node2Label extends NodeLabel {
  final def apply(children: Seq[Term]): Term = {
    assert(children.size == 2)
    apply(children(0), children(1))
  }
  def apply(_1: Term, _2: Term): Term
}
trait Node3Label extends NodeLabel {
  final def apply(children: Seq[Term]): Term = {
    assert(children.size == 3)
    apply(children(0), children(1), children(2))
  }
  def apply(_1: Term, _2: Term, _3: Term): Term
}

trait Node0 extends Node {
  val label: Node0Label
  def iterator = Iterator.empty
  override def map(f: (Term) => Term): Term = label()
}
object Node0 {
  def apply(label: Node0Label) = Node0Implementation(label)
}
case class Node0Implementation(label: Node0Label) extends Node0

case class Node1(label: Node1Label, _1: Term) extends Node with Product1[Term] {
  def iterator = Iterator(_1)
  override def map(f: (Term) => Term): Term = label(f(_1))
}
trait Node2 extends Node with Product2[Term, Term] {
  val label: Node2Label
  def iterator = Iterator(_1, _2)
  override def map(f: (Term) => Term): Term = label(f(_1), f(_2))
}
object Node2 {
  def apply(label: Node2Label, _1: Term, _2: Term) = Node2Implementation(label, _1, _2)
}
case class Node2Implementation(label: Node2Label, _1: Term, _2: Term) extends Node2
case class Node3(label: Node3Label, _1: Term, _2: Term, _3: Term) extends Node {
  def iterator = Iterator(_1, _2, _3)
  override def map(f: (Term) => Term): Term = label(f(_1), f(_2), f(_3))
}
trait CollectionLabel[C <: IterableLike[Term, C]] extends NodeLabel {
  implicit val bf: CanBuildFrom[C, Term, C]
  val unit: CollectionUnitLabel[C]
  val op: CollectionOpLabel[C]
  def apply(): Term = EmptyCollectionNode(unit)
  def apply(t: Term): CollectionNode[C] = t.label match {
    case `unit` | `op` => t.asInstanceOf[CollectionNode[C]]
    case _ =>
      val builder = bf()
      builder.+=(t)
      op(builder.result())
  }
}
trait CollectionOpLabel[C <: IterableLike[Term, C]] extends Node2Label with CollectionLabel[C] {
  val unit: CollectionUnitLabel[C]
  val op = this
  def apply(s: C) = NeCollectionNode(op, s)
}
trait CollectionUnitLabel[C <: IterableLike[Term, C]] extends Node0Label {
  implicit val bf: CanBuildFrom[C, Term, C]
  val op: CollectionOpLabel[C]
  val unit = this
}
trait CollectionNode[C <: IterableLike[Term, C]] extends Node {
  val collection: C
}
case class EmptyCollectionNode[C <: IterableLike[Term, C]](label: CollectionUnitLabel[C]) extends Node0
case class NeCollectionNode[C <: IterableLike[Term, C]](label: CollectionOpLabel[C], collection: C)
  extends CollectionNode[C] with Node2 {
  override val _1: Term = collection.head
  override val _2: Term = if (collection.size == 1) label.unit() else label(collection.tail)
  override def map(f: (Term) => Term): Term = label(collection.map(f)(label.bf))
}

object Node {
  def apply(label: Node0Label) = Node0(label)
  def apply(label: Node1Label, _1: Term) = Node1(label, _1)
  def apply(label: Node2Label, _1: Term, _2: Term) = Node2(label, _1, _2)
  def apply(label: Node3Label, _1: Term, _2: Term, _3: Term) = Node3(label, _1, _2, _3)
}

case class Operator0Label[T](module: Module, name: String, elm: TokenLabel[T], f: () => T) extends Node0Label {
  def apply(): Term = elm(f())
}
trait Function1Label[A, R] extends Node1Label {
  val aLabel: TokenLabel[A]
  val rLabel: TokenLabel[R]
  val f: A => R
  def apply(_1: Term): Term = _1 match {
    case aLabel(a) => rLabel(f(a))
    case _ => Node(this, _1)
  }
}
case class Operator1Label[T](module: Module, name: String, elmLabel: TokenLabel[T], f: T => T)
  extends Function1Label[T, T] {
  val aLabel = elmLabel
  val rLabel = elmLabel
}
trait Function2Label[A, B, R] extends Node2Label {
  val aLabel: TokenLabel[A]
  val bLabel: TokenLabel[B]
  val rLabel: TokenLabel[R]
  val f: (A, B) => R
  def apply(_1: Term, _2: Term): Term = (_1, _2) match {
    case (aLabel(a), bLabel(b)) => rLabel(f(a, b))
    case _ => Node(this, _1, _2)
  }
}
case class Operator2Label[T](module: Module, name: String, elmLabel: TokenLabel[T], f: (T, T) => T)
  extends Function2Label[T, T, T] {
  val aLabel = elmLabel
  val rLabel = elmLabel
  val bLabel = elmLabel
}

object LOGIC extends Module {
  object Variable extends LeafLabel[String] with InModule with NameFromObject
  case class Variable(value: String) extends Leaf[String] {
    override val label: LeafLabel[String] = Variable
  }
}

object SET extends Module {
  trait SetLabel extends CollectionLabel[Set[Term]] {
    lazy val bf = implicitly[CanBuildFrom[Set[Term], Term, Set[Term]]]
  }
  object SetOp extends CollectionOpLabel[Set[Term]] with InModule with SetLabel {
    lazy val unit: CollectionUnitLabel[Set[Term]] = SetUnit
    override def apply(_1: Term, _2: Term): Term = op(op(_1).collection ++ op(_2).collection)
    val name: String = "SetOp"
  }
  object SetUnit extends CollectionUnitLabel[Set[Term]] with InModule with SetLabel {
    val name: String = "SetUnit"
    lazy val op: CollectionOpLabel[Set[Term]] = SetOp
  }
}

object INT extends Module {
  val Int = TokenLabel[scala.Int](this, "Int")
  val + = Operator2Label[scala.Int](this, "+", Int, _ + _)
  val - = Operator2Label[scala.Int](this, "-", Int, _ - _)
}
