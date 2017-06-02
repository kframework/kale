package org.kframework.kale

import org.kframework.kale.standard._

trait FunctionLabel {
  val name: String
}

trait PureFunctionLabel {
  self: FunctionLabel =>
}

trait FunctionLabel0 extends Label0 with FunctionLabel {
  def f(): Option[Term]

  override def apply(): Term = f() getOrElse FreeNode0(this)
}

trait FunctionLabel1 extends Label1 with FunctionLabel {
  def f(_1: Term): Option[Term]

  override def apply(_1: Term): Term = env.bottomize(_1) {
    f(_1) getOrElse FreeNode1(this, _1)
  }
}

trait FunctionLabel2 extends Label2 with FunctionLabel {
  def f(_1: Term, _2: Term): Option[Term]

  override def apply(_1: Term, _2: Term): Term = env.bottomize(_1, _2) {
    f(_1, _2) getOrElse FreeNode2(this, _1, _2)
  }
}

trait FunctionLabel3 extends Label3 with FunctionLabel {
  def f(_1: Term, _2: Term, _3: Term): Option[Term]

  override def apply(_1: Term, _2: Term, _3: Term): Term = env.bottomize(_1, _2, _3) {
    f(_1, _2, _3) getOrElse FreeNode3(this, _1, _2, _3)
  }
}

trait FunctionLabel4 extends Label4 with FunctionLabel {
  def f(_1: Term, _2: Term, _3: Term, _4: Term): Option[Term]

  override def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term = env.bottomize(_1, _2, _3, _4) {
    f(_1, _2, _3, _4) getOrElse FreeNode4(this, _1, _2, _3, _4)
  }
}

case class PrimitiveFunction1[A, R](name: String, aLabel: LeafLabel[A], rLabel: LeafLabel[R], primitiveF: A => R)(implicit val env: Environment) extends FunctionLabel1 with PureFunctionLabel {
  def f(_1: Term): Option[Term] = _1 match {
    case aLabel(a) => Some(rLabel(primitiveF(a)))
    case _ => None
  }
}

object PrimitiveFunction1 {
  def apply[A](name: String, aLabel: LeafLabel[A], f: A => A)(implicit env: Environment): PrimitiveFunction1[A, A] =
    PrimitiveFunction1(name, aLabel, aLabel, f)
}

case class PrimitiveFunction2[A, B, R](name: String, aLabel: LeafLabel[A], bLabel: LeafLabel[B], rLabel: LeafLabel[R], primitiveF: (A, B) => R)(implicit val env: Environment) extends FunctionLabel2 with PureFunctionLabel {
  def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
    case (aLabel(a), bLabel(b)) => Some(rLabel(primitiveF(a, b)))
    case _ => None
  }
}

object PrimitiveFunction2 {
  def apply[A, R](name: String, aLabel: LeafLabel[A], rLabel: LeafLabel[R], f: (A, A) => R)(implicit env: Environment): PrimitiveFunction2[A, A, R] =
    PrimitiveFunction2(name, aLabel, aLabel, rLabel, f)

  def apply[A](name: String, aLabel: LeafLabel[A], f: (A, A) => A)(implicit env: Environment): PrimitiveFunction2[A, A, A] =
    PrimitiveFunction2(name, aLabel, aLabel, aLabel, f)
}

case class PrimitiveFunction3[A, B, C, R](name: String, aLabel: LeafLabel[A], bLabel: LeafLabel[B], cLabel: LeafLabel[C], rLabel: LeafLabel[R], primitiveF: (A, B, C) => R)(implicit val env: Environment) extends FunctionLabel3 with PureFunctionLabel {
  def f(_1: Term, _2: Term, _3: Term): Option[Term] = (_1, _2, _3) match {
    case (aLabel(a), bLabel(b), cLabel(c)) => Some(rLabel(primitiveF(a, b, c)))
    case _ => None
  }
}

object PrimitiveFunction3 {
  def apply[A, R](name: String, aLabel: LeafLabel[A], rLabel: LeafLabel[R], f: (A, A, A) => R)(implicit env: Environment): PrimitiveFunction3[A, A, A, R] =
    PrimitiveFunction3(name, aLabel, aLabel, aLabel, rLabel, f)

  def apply[A](name: String, aLabel: LeafLabel[A], f: (A, A, A) => A)(implicit env: Environment): PrimitiveFunction3[A, A, A, A] =
    PrimitiveFunction3(name, aLabel, aLabel, aLabel, aLabel, f)
}
