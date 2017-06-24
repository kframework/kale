package org.kframework.kale

import org.kframework.kale.standard._
import org.kframework.kale.transformer.Binary

trait MatcherOrUnifier extends transformer.Binary.Apply {
  val env: standard.MatchingLogic

  import Binary._
  import env._


  def FreeNode0FreeNode0(solver: Apply)(a: Node0, b: Node0) = Next(b)

  def FreeNode1FreeNode1(solver: Apply)(a: Node1, b: Node1): Term = And.combine(b)(Task(a._1, b._1))

  def FreeNode2FreeNode2(solver: Apply)(a: Node2, b: Node2): Term = And.combine(b)(Task(a._1, b._1), Task(a._2, b._2))

  def FreeNode3FreeNode3(solver: Apply)(a: Node3, b: Node3): Term = And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3))

  def FreeNode4FreeNode4(solver: Apply)(a: Node4, b: Node4): Term = And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4))

  def FreeNode5FreeNode5(solver: Apply)(a: Node5, b: Node5): Term = And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4), Task(a._5, b._5))

  def FreeNode6FreeNode6(solver: Apply)(a: Node6, b: Node6): Term = And.combine(b)(Task(a._1, b._1), Task(a._2, b._2), Task(a._3, b._3), Task(a._4, b._4), Task(a._5, b._5), Task(a._6, b._6))

  def Constants(solver: Apply)(a: DomainValue[_], b: DomainValue[_]) =
    And(Truth(a.data == b.data), Next(b))

  def NoMatch(solver: Apply)(a: Term, b: Term): Term = Bottom

  val freeLabelProcessing = definePartialFunction({
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 => NoMatch _
    case (_: FreeLabel0, _: FreeLabel0) => FreeNode0FreeNode0
    case (_: FreeLabel1, _: FreeLabel1) => FreeNode1FreeNode1
    case (_: FreeLabel2, _: FreeLabel2) => FreeNode2FreeNode2
    case (_: FreeLabel3, _: FreeLabel3) => FreeNode3FreeNode3
    case (_: FreeLabel4, _: FreeLabel4) => FreeNode4FreeNode4
  })

  def FunctionDefinedByRewritingMatcher(solver: Apply)(a: Term, b: Term) = {
    val l = a.label.asInstanceOf[FunctionDefinedByRewriting]
    And(Next(b), And(a.children.zip(b.children).map({
      case (ca, cb) => solver(ca, cb) match {
        case And.withNext(p, _) => p
      }
    })))
  }

  val functionDefinedByRewritingProcessing = definePartialFunction({
    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FunctionDefinedByRewritingMatcher _
    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FunctionDefinedByRewritingMatcher _
    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FunctionDefinedByRewritingMatcher _
    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FunctionDefinedByRewritingMatcher _
    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FunctionDefinedByRewritingMatcher _
  })
}
