package org.kframework.kale

import org.kframework.kale.standard._
import org.kframework.kale.transformer.Binary

trait MatcherOrUnifier extends transformer.Binary.Apply {
  val env: standard.MatchingLogicMixin

  import Binary._
  import env._




  def Constants(solver: Apply)(a: DomainValue[_], b: DomainValue[_]) =
    And(Truth(a.data == b.data), Next(b))

  case class FunctionDefinedByRewritingMatcher(solver: Apply) extends Binary.F({(a: Term, b: Term) => {
    val l = a.label.asInstanceOf[FunctionDefinedByRewriting]
    And(Next(b), And(a.children.zip(b.children).map({
      case (ca, cb) => solver(ca, cb) match {
        case And.withNext(p, _) => p
      }
    })))
  }})

  val functionDefinedByRewritingProcessing = Binary.definePartialFunction({
    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FunctionDefinedByRewritingMatcher
  })
}
