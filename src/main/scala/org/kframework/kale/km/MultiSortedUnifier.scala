package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunction, TypedWith}
import org.kframework.kale._
import org.kframework.kale.standard.{SimpleRewrite, SingleSortedMatcher}

class MultiSortedUnifier(val env: KMEnvironment) extends kale.MatcherOrUnifier {

  import env._

  object SortedVarLeft extends ProcessingFunction[Apply] with TypedWith[Variable, Term] {
    def f(solver: Apply)(a: Variable, b: Term) =
      if (a.sort == b.sort)
        VarLeft(solver)(a, b)
      else
        Bottom
  }

  object SortedVarRight extends ProcessingFunction[Apply] with TypedWith[Term, Variable] {
    def f(solver: Apply)(a: Term, b: Variable) = SortedVarLeft.f(solver)(b, a)
  }

  def RewriteMatcher(solver: kale.MatcherOrUnifier)(a: SimpleRewrite, b: Term): Term = {
    solver(a._1, b)
  }

  import kale.standard._

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case (Rewrite, _) => RewriteMatcher _
    case (Variable, _) => SortedVarLeft
    case (_, Variable) => SortedVarRight
    case (And, _) => AndTerm _
    case (_, And) => TermAnd _
    case (Or, _) => OrTerm _
    case (_, Or) => TermOr _
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 || env.sortTarget(l1) != env.sortTarget(l2) => NoMatch _
  })
    .orElse(freeLabelProcessing)
    .orElse(functionDefinedByRewritingProcessing)
    .orElse(definePartialFunction({
      case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants _
      //    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
      //    case (_: AssocLabel, right) if !right.isInstanceOf[Variable] => AssocTerm
    }))
    .orElse(super.processingFunctions)
}
