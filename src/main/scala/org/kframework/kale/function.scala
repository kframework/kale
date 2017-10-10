package org.kframework.kale

import cats.Monoid
import org.kframework.kale.standard.AssocWithIdList
import org.roaringbitmap.RoaringBitmap

import scala.annotation.switch

trait FunctionLabel extends NodeLabel with CluelessRoaring {
  val name: String
}

trait PureFunctionLabel {
  self: FunctionLabel =>

  override lazy val isPredicate = Some(false)
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

case class PrimitiveFunction1[A, R](name: String, aLabel: UpDown[A], rLabel: Up[R], primitiveF: A => R)(implicit val env: Environment) extends FunctionLabel1 with PureFunctionLabel {
  def f(_1: Term): Option[Term] = _1 match {
    case aLabel(a) => Some(rLabel(primitiveF(a)))
    case _ => None
  }
}

object PrimitiveFunction1 {
  def apply[A](name: String, aLabel: UpDown[A], f: A => A)(implicit env: Environment): PrimitiveFunction1[A, A] =
    PrimitiveFunction1(name, aLabel, aLabel, f)
}

case class PrimitiveFunction2[A, B, R](name: String, aLabel: UpDown[A], bLabel: UpDown[B], rLabel: Up[R], primitiveF: (A, B) => R)(implicit val env: Environment)
  extends FunctionLabel2 with PureFunctionLabel {

  def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
    case (aLabel(a), bLabel(b)) => Some(rLabel(primitiveF(a, b)))
    case _ => None
  }
}

case class PrimitiveMonoid[O: Monoid : UpDown](name: String)(implicit val env: Environment)
  extends FunctionLabel with PureFunctionLabel with MonoidLabel {

  val primitiveMonoid: Monoid[O] = implicitly[Monoid[O]]
  val updown: UpDown[O] = implicitly[UpDown[O]]

  private val Self = this

  def combineExtendedWithABitOfSymbolic(a: Term, b: Term): Option[Term] = ((a, b): @switch) match {
    case (`identity`, b) => Some(b)
    case (a, `identity`) => Some(a)
    case (updown(aPrimitive), updown(bPrimitive)) => Some(updown(primitiveMonoid.combine(aPrimitive, bPrimitive)))
    case _ => None
  }

  /**
    * take the last of la and use it to eat away at lb
    */
  def processB(el: Term, oldList: Vector[Term]): Vector[Term] = {
    combineExtendedWithABitOfSymbolic(el, oldList.head) match {
      case Some(newEl) => processB(newEl, oldList.tail)
      case None => el +: oldList
    }
  }

  /**
    * now take the first of the newBList and use it to eat away at la
    */
  def processA(oldList: Vector[Term], el: Term): Vector[Term] = {
    combineExtendedWithABitOfSymbolic(oldList.last, el) match {
      case Some(newEl) => processA(oldList.dropRight(1), newEl)
      case None => oldList :+ el
    }
  }

  def apply(a: Term, b: Term) = (a, b) match {
    case (AssocWithIdList(Self, la: Vector[Term]), AssocWithIdList(Self, lb: Vector[Term])) => {
      val newBList = processB(la.last, lb)
      val newAList = processA(la, newBList.head)
      // the solution is the concatenation of the two, minding the gap
      AssocWithIdList(this, newAList ++ newBList.tail)
    }
    case (a, AssocWithIdList(Self, lb: Vector[Term])) => {
      val newBList = processB(a, lb)
      AssocWithIdList(this, newBList)
    }
    case (AssocWithIdList(Self, la: Vector[Term]), b) => {
      val newAList = processA(la, b)
      AssocWithIdList(this, newAList)
    }
    case (a, b) => combineExtendedWithABitOfSymbolic(a, b) getOrElse AssocWithIdList(this, List(a, b))
  }

  override val identity =
    updown.apply(primitiveMonoid.empty)
}

//case class PrimitiveMonoid[O](label: PrimitiveMonoidLabel[O], list: List[O]) extends Node2 {
//  override def _1 = label.updown(list.head)
//
//  override def _2 = {
//    val tail = list.tail
//    if (tail.size == 1) {
//      label.updown(tail.head)
//    } else {
//      assert(tail.size > 1)
//      PrimitiveMonoid(label, tail)
//    }
//  }
//}


object PrimitiveFunction2 {
  def apply[A, R](name: String, aLabel: UpDown[A], rLabel: Up[R], f: (A, A) => R)(implicit env: Environment): PrimitiveFunction2[A, A, R] =
    PrimitiveFunction2(name, aLabel, aLabel, rLabel, f)

  def apply[A](name: String, aLabel: UpDown[A], f: (A, A) => A)(implicit env: Environment): PrimitiveFunction2[A, A, A] =
    PrimitiveFunction2(name, aLabel, aLabel, aLabel, f)
}

case class PrimitiveFunction3[A, B, C, R](name: String, aLabel: UpDown[A], bLabel: UpDown[B], cLabel: UpDown[C], rLabel: Up[R], primitiveF: (A, B, C) => R)(implicit val env: Environment) extends FunctionLabel3 with PureFunctionLabel {
  def f(_1: Term, _2: Term, _3: Term): Option[Term] = (_1, _2, _3) match {
    case (aLabel(a), bLabel(b), cLabel(c)) => Some(rLabel(primitiveF(a, b, c)))
    case _ => None
  }
}

object PrimitiveFunction3 {
  def apply[A, R](name: String, aLabel: UpDown[A], rLabel: UpDown[R], f: (A, A, A) => R)(implicit env: Environment): PrimitiveFunction3[A, A, A, R] =
    PrimitiveFunction3(name, aLabel, aLabel, aLabel, rLabel, f)

  def apply[A](name: String, aLabel: UpDown[A], f: (A, A, A) => A)(implicit env: Environment): PrimitiveFunction3[A, A, A, A] =
    PrimitiveFunction3(name, aLabel, aLabel, aLabel, aLabel, f)
}
