package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply

import scala.language.implicitConversions

trait FreeMixin extends kale.FreeMixin with HasMatcher with HasUnifier {
  self: Environment =>
  override def FreeLabel0(name: String): FreeLabel0 = ???

  override def FreeLabel1(name: String): FreeLabel1 = ???

  override def FreeLabel2(name: String): FreeLabel2 = ???

  override def FreeLabel3(name: String): FreeLabel3 = ???

  override def FreeLabel4(name: String): FreeLabel4 = ???

  override def FreeLabel5(name: String): FreeLabel5 = ???

  override def FreeLabel6(name: String): FreeLabel6 = ???

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

case class SimpleFreeLabel0 private(name: String)(implicit val env: Environment) extends FreeLabel0

case class SimpleFreeLabel1 private(name: String)(implicit val env: Environment) extends FreeLabel1

case class SimpleFreeLabel2 private(name: String)(implicit val env: Environment) extends FreeLabel2

case class SimpleFreeLabel3 private(name: String)(implicit val env: Environment) extends FreeLabel3

case class SimpleFreeLabel4 private(name: String)(implicit val env: Environment) extends FreeLabel4
