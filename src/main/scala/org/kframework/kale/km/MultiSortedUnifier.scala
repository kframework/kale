package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunction, TypedWith}
import org.kframework.kale._

class MultiSortedUnifier(val env: KMEnvironment) extends kale.MatcherOrUnifier {
  import env._

  object SortedVarLeft extends ProcessingFunction[Apply] with TypedWith[Variable, Term] {
    /*
      This sorted unifier does the occur-check, even if org.kframework.kale.standard.StandardEqualityLabel.apply also does it.
      Note that StandardEqualityLabel.apply simply returns Equals instead of Bottom when the occur-check is failed,
      which is more flexible in the sense that the returned Equals could be either reduced to Bottom later by external solvers (e.g., z3),
      or used in other ways (e.g., infinite trace solvers).
     */
    def f(solver: Apply)(a: Variable, b: Term) =
      if(a == b)
        Top
      else if(a.sort == b.sort && !util.Util.contains(b,a))
        VarLeft.f(solver)(a,b)
      else
        Bottom
  }

  object SortedVarRight extends ProcessingFunction[Apply] with TypedWith[Term, Variable] {
    def f(solver: Apply)(a: Term, b: Variable) = SortedVarLeft.f(solver)(b,a)
//      if(a == b)
//        Top
//      else if(a.sort == b.sort && !util.Util.contains(b,a))
//        VarRight.f(solver)(a,b)
//      else
//        Bottom
  }

  import kale.standard._

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case (Variable, _) => SortedVarLeft
    case (_, Variable) => SortedVarRight
    case (And, _) => AndTerm
    case (_, And) => TermAnd
    case (Or, _) => OrTerm
    case (_, Or) => TermOr
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 || env.targetSort(l1) != env.targetSort(l2) => NoMatch
    case (_: FreeLabel0, _: FreeLabel0) => FreeNode0FreeNode0
    case (_: FreeLabel1, _: FreeLabel1) => FreeNode1FreeNode1
    case (_: FreeLabel2, _: FreeLabel2) => FreeNode2FreeNode2
    case (_: FreeLabel3, _: FreeLabel3) => FreeNode3FreeNode3
    case (_: FreeLabel4, _: FreeLabel4) => FreeNode4FreeNode4
    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FreeNode0FreeNode0
    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FreeNode1FreeNode1
    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FreeNode2FreeNode2
    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FreeNode3FreeNode3
    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FreeNode4FreeNode4
    case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants
//    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
//    case (_: AssocLabel, right) if !right.isInstanceOf[Variable] => AssocTerm
  }) orElse super.processingFunctions
}
