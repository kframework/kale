package org.kframework.kale

import org.kframework.kale.standard.DNFEnvironment
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

  object FreeNode0FreeNode0 extends ProcessingFunction[Apply] with TypedWith[Node0, Node0] {
    def f(solver: Apply)(a: Node0, b: Node0) = Top
  }

  object FreeNode1FreeNode1 extends ProcessingFunction[Apply] with TypedWith[Node1, Node1] {
    def f(solver: Apply)(a: Node1, b: Node1) = shortCircuitAnd(solver)((a._1, b._1))
  }

  object FreeNode2FreeNode2 extends ProcessingFunction[Apply] with TypedWith[Node2, Node2] {
    def f(solver: Apply)(a: Node2, b: Node2) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2))
  }

  object FreeNode3FreeNode3 extends ProcessingFunction[Apply] with TypedWith[Node3, Node3] {
    def f(solver: Apply)(a: Node3, b: Node3) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3))
  }

  object FreeNode4FreeNode4 extends ProcessingFunction[Apply] with TypedWith[Node4, Node4] {
    def f(solver: Apply)(a: Node4, b: Node4) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3), (a._4, b._4))
  }

  object FreeNode5FreeNode5 extends ProcessingFunction[Apply] with TypedWith[Node5, Node5] {
    def f(solver: Apply)(a: Node5, b: Node5) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3), (a._4, b._4), (a._5, b._5))
  }

  object FreeNode6FreeNode6 extends ProcessingFunction[Apply] with TypedWith[Node6, Node6] {
    def f(solver: Apply)(a: Node6, b: Node6) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3), (a._4, b._4), (a._5, b._5), (a._6, b._6))
  }

  object VarLeft extends ProcessingFunction[Apply] with TypedWith[Variable, Term] {
    def f(solver: Apply)(a: Variable, b: Term) = Equality(a.asInstanceOf[Variable], b)
  }

  object VarRight extends ProcessingFunction[Apply] with TypedWith[Term, Variable] {
    def f(solver: Apply)(a: Term, b: Variable) = Equality(b.asInstanceOf[Variable], a)
  }

  object Constants extends ProcessingFunction[Apply] with TypedWith[DomainValue[_], DomainValue[_]] {
    override def f(solver: Apply)(a: DomainValue[_], b: DomainValue[_]) =
      Truth(a.data == b.data)
  }

  object AndTerm extends ProcessingFunction[Apply] with TypedWith[And, Term] {
    override def f(solver: Apply)(a: And, b: Term): Term = {
      val solution = solver(a.nonFormula.get, b)
      And(a.formulas, solution)
    }
  }

  object TermAnd extends ProcessingFunction[Apply] with TypedWith[Term, And] {
    override def f(solver: Apply)(a: Term, b: And): Term = {
      val solution = solver(a, b.nonFormula.get)
      And(solution, b.formulas)
    }
  }

  object OrTerm extends ProcessingFunction[Apply] with TypedWith[Or, Term] {
    def f(solver: Apply)(a: Or, b: Term) = {
      val sol = a.asSet map (solver(_, b))
      Or(sol)
    }
  }

  object TermOr extends ProcessingFunction[Apply] with TypedWith[Term, Or] {
    def f(solver: Apply)(a: Term, b: Or) = {
      val sol = b.asSet map (solver(a, _))
      Or(sol)
    }
  }

  object NoMatch extends ProcessingFunction[Apply] with TypedWith[Term, Term] {
    override def f(solver: Apply)(a: Term, b: Term): Term = Bottom
  }

}
