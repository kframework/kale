package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.LabelNamed

import scala.language.implicitConversions

trait FreeMixin extends kale.FreeMixin {
  _: Environment =>
  override def FreeLabel0(name: String): FreeLabel0 = new LabelNamed(name) with FreeLabel0

  override def FreeLabel1(name: String): FreeLabel1 = new LabelNamed(name) with FreeLabel1

  override def FreeLabel2(name: String): FreeLabel2 = new LabelNamed(name) with FreeLabel2

  override def FreeLabel3(name: String): FreeLabel3 = new LabelNamed(name) with FreeLabel3

  override def FreeLabel4(name: String): FreeLabel4 = new LabelNamed(name) with FreeLabel4

  override def FreeLabel5(name: String): FreeLabel5 = new LabelNamed(name) with FreeLabel5

  override def FreeLabel6(name: String): FreeLabel6 = new LabelNamed(name) with FreeLabel6

  override def FreeLabelN(name: String, theArity: Int): FreeLabelN = new LabelNamed(name) with FreeLabelN {
    override val arity: Int = theArity
  }

  case class FreeNode0FreeNode0(solver: Apply) extends Binary.F({ (a: Node0, b: Node0) => b })

  case class FreeNode1FreeNode1(solver: Apply) extends Binary.F({ (a: Node1, b: Node1) =>
    And.combine(b)(Task(a._1, b._1))
  })

  case class FreeNode2FreeNode2(solver: Apply) extends Binary.F({ (a: Node2, b: Node2) =>
    And.combine(b)(Task(a._1, b._1), Task(a._2, b._2))
  })

  case class FreeNode3FreeNode3(solver: Apply) extends Binary.F({ (a: Node3, b: Node3) =>
    And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3))
  })

  case class FreeNode4FreeNode4(solver: Apply) extends Binary.F({ (a: Node4, b: Node4) =>
    And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4))
  })

  case class FreeNode5FreeNode5(solver: Apply) extends Binary.F({ (a: Node5, b: Node5) =>
    And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4), Task(a._5, b._5))
  })

  case class FreeNode6FreeNode6(solver: Apply) extends Binary.F({ (a: Node6, b: Node6) =>
    And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4), Task(a._5, b._5), Task(a._6, b._6))
  })

  case class FreeNodeNFreeNodeN(solver: Apply) extends Binary.F({ (a: NodeN, b: NodeN) =>
    And.combine(b)(a.children.zip(b.children) map { case (ac, bc) => Task(ac, bc) }: _*)
  })

  registerMatcher({
    case (a: FreeLabel0, b: FreeLabel0) if a == b => FreeNode0FreeNode0
    case (a: FreeLabel1, b: FreeLabel1) if a == b => FreeNode1FreeNode1
    case (a: FreeLabel2, b: FreeLabel2) if a == b => FreeNode2FreeNode2
    case (a: FreeLabel3, b: FreeLabel3) if a == b => FreeNode3FreeNode3
    case (a: FreeLabel4, b: FreeLabel4) if a == b => FreeNode4FreeNode4
    case (a: FreeLabel5, b: FreeLabel5) if a == b => FreeNode5FreeNode5
    case (a: FreeLabel6, b: FreeLabel6) if a == b => FreeNode6FreeNode6
    case (a: FreeLabelN, b: FreeLabelN) if a == b => FreeNodeNFreeNodeN
  }, Priority.low)
}

trait TuplesMixin extends Mixin {
  _: Environment with FreeMixin =>
  val Tuple0 = FreeLabel0("Tuple0")
  val Tuple1 = FreeLabel1("Tuple1")
  val Tuple2 = FreeLabel2("Tuple2")
  val Tuple3 = FreeLabel3("Tuple3")
  val Tuple4 = FreeLabel4("Tuple4")
  val Tuple5 = FreeLabel5("Tuple5")
  val Tuple6 = FreeLabel6("Tuple6")
}
