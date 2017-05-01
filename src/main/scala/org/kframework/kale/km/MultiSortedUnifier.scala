package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunction, TypedWith}
import org.kframework.kale._

class MultiSortedUnifier(val env: KMEnvironment) extends kale.MatcherOrUnifier {
  import env._

  object SortedVarLeft extends ProcessingFunction[Apply] with TypedWith[Variable, Term] {
    def f(solver: Apply)(a: Variable, b: Term) =
      if(a == b)
        Top
      else if(a.sort == b.sort)
        VarLeft(solver)(a,b)
      else
        Bottom
  }

  object SortedVarRight extends ProcessingFunction[Apply] with TypedWith[Term, Variable] {
    def f(solver: Apply)(a: Term, b: Variable) = SortedVarLeft.f(solver)(b,a)
  }

  import kale.standard._

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case (Variable, _) => SortedVarLeft
    case (_, Variable) => SortedVarRight
    case (And, _) => AndTerm _
    case (_, And) => TermAnd _
    case (Or, _) => OrTerm _
    case (_, Or) => TermOr _
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 || env.targetSort(l1) != env.targetSort(l2) => NoMatch _
    case (_: SimpleFreeLabel0, _: SimpleFreeLabel0) => FreeNode0FreeNode0 _
    case (_: SimpleFreeLabel1, _: SimpleFreeLabel1) => FreeNode1FreeNode1 _
    case (_: SimpleFreeLabel2, _: SimpleFreeLabel2) => FreeNode2FreeNode2 _
    case (_: SimpleFreeLabel3, _: SimpleFreeLabel3) => FreeNode3FreeNode3 _
    case (_: SimpleFreeLabel4, _: SimpleFreeLabel4) => FreeNode4FreeNode4 _
    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FreeNode0FreeNode0 _
    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FreeNode1FreeNode1 _
    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FreeNode2FreeNode2 _
    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FreeNode3FreeNode3 _
    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FreeNode4FreeNode4 _
    case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants _
//    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
//    case (_: AssocLabel, right) if !right.isInstanceOf[Variable] => AssocTerm
  }) orElse super.processingFunctions
}
