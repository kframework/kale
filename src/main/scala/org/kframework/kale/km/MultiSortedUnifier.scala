package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.SimpleRewrite
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply

class MultiSortedUnifier(val env: KMEnvironment) extends kale.MatcherOrUnifier {

  import env._

  def SortedVarLeft(solver: Apply)(a: Variable, b: Term): Term =
    if (a.sort == b.sort)
      VarLeft(solver)(a, b)
    else
      Bottom

  def SortedVarRight(solver: Apply)(a: Term, b: Variable) = SortedVarLeft(solver)(b, a)

  def RewriteMatcher(solver: kale.MatcherOrUnifier)(a: SimpleRewrite, b: Term) = solver(a._1, b)

  override def processingFunctions: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (Rewrite, _) => RewriteMatcher _
    case (Variable, _) => SortedVarLeft
    case (_, Variable) => SortedVarRight _
    case (And, _) => AndTerm
    case (_, And) => TermAnd
    case (Or, _) => OrTerm
    case (_, Or) => TermOr
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 || env.sortTarget(l1) != env.sortTarget(l2) => NoMatch
  })
    .orElse(freeLabelProcessing)
    .orElse(functionDefinedByRewritingProcessing)
    .orElse(Binary.definePartialFunction({
      case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants _
      //    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
      //    case (_: AssocLabel, right) if !right.isInstanceOf[Variable] => AssocTerm
    }))
    .orElse(super.processingFunctions)
}
