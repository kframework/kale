package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.Named

import scala.language.implicitConversions

trait FreeMixin extends kale.FreeMixin with HasMatcher with HasUnifier {
  self: Environment =>
  override def FreeLabel0(name: String): FreeLabel0 = new Named(name) with FreeLabel0

  override def FreeLabel1(name: String): FreeLabel1 = new Named(name) with FreeLabel1

  override def FreeLabel2(name: String): FreeLabel2 = new Named(name) with FreeLabel2

  override def FreeLabel3(name: String): FreeLabel3 = new Named(name) with FreeLabel3

  override def FreeLabel4(name: String): FreeLabel4 = new Named(name) with FreeLabel4

  override def FreeLabel5(name: String): FreeLabel5 = new Named(name) with FreeLabel5

  override def FreeLabel6(name: String): FreeLabel6 = new Named(name) with FreeLabel6

  case class FreeNode0FreeNode0(solver: Apply) extends Binary.F({(a: Node0, b: Node0) => Next(b)})

  case class FreeNode1FreeNode1(solver: Apply) extends Binary.F({(a: Node1, b: Node1) => And.combine(b)(Task(a._1, b._1))})

  case class FreeNode2FreeNode2(solver: Apply) extends Binary.F({(a: Node2, b: Node2) => And.combine(b)(Task(a._1, b._1), Task(a._2, b._2))})

  case class FreeNode3FreeNode3(solver: Apply) extends Binary.F({(a: Node3, b: Node3) => And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3))})

  case class FreeNode4FreeNode4(solver: Apply) extends Binary.F({(a: Node4, b: Node4) => And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4))})

  case class FreeNode5FreeNode5(solver: Apply) extends Binary.F({(a: Node5, b: Node5) => And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4), Task(a._5, b._5))})

  case class FreeNode6FreeNode6(solver: Apply) extends Binary.F({(a: Node6, b: Node6) => And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4), Task(a._5, b._5), Task(a._6, b._6))})

  case class NoMatch(solver: Apply) extends Binary.F({(a: Term, b: Term) => Bottom})

  override protected def makeMatcher = Binary.definePartialFunction({
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 || env.sort(l1) != env.sort(l2) => NoMatch
    case (_: FreeLabel0, _: FreeLabel0) => FreeNode0FreeNode0
    case (_: FreeLabel1, _: FreeLabel1) => FreeNode1FreeNode1
    case (_: FreeLabel2, _: FreeLabel2) => FreeNode2FreeNode2
    case (_: FreeLabel3, _: FreeLabel3) => FreeNode3FreeNode3
    case (_: FreeLabel4, _: FreeLabel4) => FreeNode4FreeNode4
  }).orElse(super.makeMatcher)
}

trait TuplesMixing extends Environment with FreeMixin {
  val Tuple1 = FreeLabel1("Tuple1")
  val Tuple2 = FreeLabel1("Tuple2")
  val Tuple3 = FreeLabel1("Tuple3")
  val Tuple4 = FreeLabel1("Tuple4")
  val Tuple5 = FreeLabel1("Tuple5")
  val Tuple6 = FreeLabel1("Tuple6")
}
