package org.kframework.kale

import org.kframework.kale.standard._
import org.kframework.kale.transformer.Binary

trait MatcherOrUnifier extends transformer.Binary.Apply {
  val env: DNFEnvironment

  import Binary._
  import env._

  def shortCircuitAnd(solver: Apply)(toEqual: (Term, Term)*): Term = {
    toEqual.foldLeft(Top: Term)({
      case (Bottom, _) => Bottom
      case (soFar, (l, r)) =>
        val results = Or.asSet(soFar) map {
          case soFarVariant@And.substitutionAndTerms(sub, _) =>
            And(soFarVariant: Term, solver(sub(l), sub(r)))
        }
        Or(results)
    })
  }

  def FreeNode0FreeNode0(solver: Apply)(a: Node0, b: Node0) = Top

  def FreeNode1FreeNode1(solver: Apply)(a: Node1, b: Node1): Term = shortCircuitAnd(solver)((a._1, b._1))

  def FreeNode2FreeNode2(solver: Apply)(a: Node2, b: Node2): Term = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2))

  def FreeNode3FreeNode3(solver: Apply)(a: Node3, b: Node3): Term = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3))

  def FreeNode4FreeNode4(solver: Apply)(a: Node4, b: Node4): Term = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3), (a._4, b._4))

  def FreeNode5FreeNode5(solver: Apply)(a: Node5, b: Node5): Term = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3), (a._4, b._4), (a._5, b._5))

  def FreeNode6FreeNode6(solver: Apply)(a: Node6, b: Node6): Term = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3), (a._4, b._4), (a._5, b._5), (a._6, b._6))

  def VarLeft(solver: Apply)(a: Variable, b: Term) = Equality(a.asInstanceOf[Variable], b)

  def VarRight(solver: Apply)(a: Term, b: Variable): Term = VarLeft(solver)(b, a) // Equality(b.asInstanceOf[Variable], a)

  def Constants(solver: Apply)(a: DomainValue[_], b: DomainValue[_]) =
    Truth(a.data == b.data)

  def AndTerm(solver: Apply)(a: And, b: Term): Term = {
    val solution = solver(a.nonPredicates.get, b)
    val fTerm = And(a.predicates, solution)
    fTerm
  }

  def TermAnd(solver: Apply)(a: Term, b: And): Term = {
    val solution = solver(a, b.nonPredicates.get)
    val finalTerm = And(solution, b.predicates)
    finalTerm
  }

  def OrTerm(solver: Apply)(a: Or, b: Term) = {
    val sol = a.asSet map (solver(_, b))
    Or(sol)
  }

  def TermOr(solver: Apply)(a: Term, b: Or) = {
    val sol = b.asSet map (solver(a, _))
    Or(sol)
  }

  def NoMatch(solver: Apply)(a: Term, b: Term): Term = Bottom

  val freeLabelProcessing = definePartialFunction({
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 => NoMatch _
    case (_: FreeLabel0, _: FreeLabel0) => FreeNode0FreeNode0 _
    case (_: FreeLabel1, _: FreeLabel1) => FreeNode1FreeNode1 _
    case (_: FreeLabel2, _: FreeLabel2) => FreeNode2FreeNode2 _
    case (_: FreeLabel3, _: FreeLabel3) => FreeNode3FreeNode3 _
    case (_: FreeLabel4, _: FreeLabel4) => FreeNode4FreeNode4 _
  })

  val functionDefinedByRewritingProcessing = definePartialFunction({
    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FreeNode0FreeNode0 _
    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FreeNode1FreeNode1 _
    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FreeNode2FreeNode2 _
    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FreeNode3FreeNode3 _
    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FreeNode4FreeNode4 _
  })
}
