package org.kframework.kale

import org.kframework.kore

import scala.collection._

trait Label0 extends (() => Term) with NodeLabel {
  val arity = 0

  def apply(): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply()

  override def toString: String =  super[NodeLabel].toString
}

trait Label1 extends (Term => Term) with NodeLabel {
  val arity = 1

  def apply(_1: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head)

  override def toString: String =  super[NodeLabel].toString
}

trait Label2 extends ((Term, Term) => Term) with NodeLabel {
  val arity = 2

  def apply(_1: Term, _2: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head)

  def unapply(t: Term): Option[(Term, Term)] = t match {
    case n: Node2 if n.label == this => Some(n._1, n._2)
    case _ => None
  }

  override def toString: String =  super[NodeLabel].toString
}

trait Label3 extends NodeLabel {
  val arity = 3

  def apply(_1: Term, _2: Term, _3: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head)

  override def toString: String =  super[NodeLabel].toString
}

trait Label4 extends NodeLabel {
  val arity = 4

  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head)

  override def toString: String =  super[NodeLabel].toString
}

trait Label5 extends NodeLabel {
  val arity = 5

  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head, l.tail.tail.tail.tail.head)

  override def toString: String =  super[NodeLabel].toString
}

trait Label6 extends NodeLabel {
  val arity = 6

  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head, l.tail.tail.tail.tail.head, l.tail.tail.tail.tail.tail.head)

  override def toString: String =  super[NodeLabel].toString
}

trait Node0 extends Node with Application {
  val label: Label0

  val isGround = true

  def innerUpdateAt(i: Int, t: Term): Term = throw new AssertionError("unreachable code")

  override def children: Iterable[Term] = Iterable.empty

  def copy(): Term = label().updatePostProcess(this)

  override def copy(children: Seq[Term]): Term = {
    assert(children.isEmpty)
    copy()
  }
}

trait Node1 extends Node with Product1[Term] {
  val label: Label1

  val isGround: Boolean = _1.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 0 => label(t)
  }

  override def children: Iterable[Term] = Iterable(_1)

  def copy(_1: Term): Term = label(_1).updatePostProcess(this)

  override def copy(children: Seq[Term]): Term = {
    assert(children.size == 1)
    copy(children.head)
  }
}

trait Node2 extends Node with Product2[Term, Term] {
  val label: Label2

  lazy val isGround: Boolean = _1.isGround && _2.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 0 => label(t, _2)
    case 1 => label(_1, t)
  }

  override def children: Iterable[Term] = Iterable(_1, _2)

  def copy(_1: Term, _2: Term): Term = label(_1, _2).updatePostProcess(this)

  override def copy(children: Seq[Term]): Term = {
    assert(children.size == 2)
    copy(children.head, children(1))
  }
}

trait Node3 extends Node with Product3[Term, Term, Term] {
  val label: Label3

  val isGround: Boolean = _1.isGround && _2.isGround && _3.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 0 => label(t, _2, _3)
    case 1 => label(_1, t, _3)
    case 2 => label(_1, _2, t)
  }

  def copy(_1: Term, _2: Term, _3: Term): Term = label(_1, _2, _3).updatePostProcess(this)

  override def copy(children: Seq[Term]): Term = {
    assert(children.size == 3)
    copy(children.head, children(1), children(2))
  }

  override def children: Iterable[Term] = Iterable(_1, _2, _3)
}

trait Node4 extends Node with Product4[Term, Term, Term, Term] {
  val label: Label4

  val isGround: Boolean = _1.isGround && _2.isGround && _3.isGround && _4.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 0 => label(t, _2, _3, _4)
    case 1 => label(_1, t, _3, _4)
    case 2 => label(_1, _2, t, _4)
    case 3 => label(_1, _2, _3, t)
  }

  def copy(_1: Term, _2: Term, _3: Term, _4: Term): Term = label(_1, _2, _3, _4).updatePostProcess(this)

  override def copy(children: Seq[Term]): Term = {
    assert(children.size == 4)
    copy(children.head, children(1), children(2), children(3))
  }

  override def children: Iterable[Term] = Iterable(_1, _2, _3, _4)
}

trait Node5 extends Node with Product5[Term, Term, Term, Term, Term] {
  val label: Label5

  val isGround: Boolean = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 0 => label(t, _2, _3, _4, _5)
    case 1 => label(_1, t, _3, _4, _5)
    case 2 => label(_1, _2, t, _4, _5)
    case 3 => label(_1, _2, _3, t, _5)
    case 4 => label(_1, _2, _3, _4, t)
  }

  def copy(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term): Term = label(_1, _2, _3, _4, _5).updatePostProcess(this)

  override def copy(children: Seq[Term]): Term = {
    assert(children.size == 5)
    copy(children.head, children(1), children(2), children(3), children(4))
  }

  override def children: Iterable[Term] = Iterable(_1, _2, _3, _4, _5)
}

trait Node6 extends Node with Product6[Term, Term, Term, Term, Term, Term] {
  val label: Label6

  val isGround: Boolean = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround && _6.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 0 => label(t, _2, _3, _4, _5, _6)
    case 1 => label(_1, t, _3, _4, _5, _6)
    case 2 => label(_1, _2, t, _4, _5, _6)
    case 3 => label(_1, _2, _3, t, _5, _6)
    case 4 => label(_1, _2, _3, _4, t, _6)
    case 5 => label(_1, _2, _3, _4, _5, t)
  }

  def copy(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term): Term = label(_1, _2, _3, _4, _5, _6).updatePostProcess(this)

  override def copy(children: Seq[Term]): Term = {
    assert(children.size == 6)
    copy(children.head, children(1), children(2), children(3), children(4), children(5))
  }

  override def children: Iterable[Term] = Iterable(_1, _2, _3, _4, _5, _6)
}
