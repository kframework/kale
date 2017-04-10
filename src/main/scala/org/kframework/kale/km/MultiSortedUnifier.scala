package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale.{Term, Variable}
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunction, TypedWith}

class MultiSortedUnifier(val env: KMEnvironment) extends kale.MatcherOrUnifier {
  import env._

  object VarLeft extends ProcessingFunction[Apply] with TypedWith[Variable, Term] {
    def f(solver: Apply)(a: Variable, b: Term) =
      if(a.sort == b.sort)
        Equality(a.asInstanceOf[Variable], b)
      else
        Bottom
  }

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case (Variable, _) => VarLeft
//    case (And, _) => AndTerm
//    case (Or, _) => OrTerm
//    case (_, Or) => TermOr
//    case (`AnywhereContext`, _) => new AnywhereContextMatcher()(env)
//    case (`CAPP`, _) => new PatternContextMatcher()(env)
//    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 => NoMatch
//    case (_: FreeLabel0, _: FreeLabel0) => FreeNode0FreeNode0
//    case (_: FreeLabel1, _: FreeLabel1) => FreeNode1FreeNode1
//    case (_: FreeLabel2, _: FreeLabel2) => FreeNode2FreeNode2
//    case (_: FreeLabel3, _: FreeLabel3) => FreeNode3FreeNode3
//    case (_: FreeLabel4, _: FreeLabel4) => FreeNode4FreeNode4
//    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FreeNode0FreeNode0
//    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FreeNode1FreeNode1
//    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FreeNode2FreeNode2
//    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FreeNode3FreeNode3
//    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FreeNode4FreeNode4
//    case (_: ConstantLabel[_], _: ConstantLabel[_]) => Constants
//    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
//    case (_: AssocLabel, right) if !right.isInstanceOf[Variable] => AssocTerm
  }) orElse super.processingFunctions
}
