package org.kframework.kale

import org.kframework.kale.transformer.Unary
import org.kframework.kale.transformer.Unary.ProcessingFunction

object BUMapper {
  def apply(processingFunction: Label => ProcessingFunction[_ <: Term, BUMapper], env: Environment)(func: PartialFunction[Term, Term]): BUMapper = new BUMapper(processingFunction, env)(func)

  def apply(env: Environment): PartialFunction[Term, Term] => BUMapper = {

    BUMapper(Unary.defaultMapping, env)
  }
}

class BUMapper(val processingFunction: Label => ProcessingFunction[_ <: Term, BUMapper], val env: Environment)(val func: PartialFunction[Term, Term]) extends Unary.Apply[BUMapper](env) with (Term => Term) {
  val liftedF = func.lift

  def apply(t: Term) =
    arr(t.label.id) match {
      case f =>
        val processedT = f(t)
        liftedF(processedT).getOrElse(processedT)
    }
}
