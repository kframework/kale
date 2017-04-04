package org.kframework.kale.util

import org.kframework.kale.transformer.Unary
import org.kframework.kale.transformer.Unary.ProcessingFunction
import org.kframework.kale.{Environment, Label, Term}

object BUMapper {
  def apply(processingFunction: Label => ProcessingFunction[BUMapper], env: Environment)(func: PartialFunction[Term, Term]): BUMapper = new BUMapper(func)(env)

  def apply(env: Environment): PartialFunction[Term, Term] => BUMapper = {

    BUMapper(env)
  }
}

class BUMapper(val func: PartialFunction[Term, Term])(implicit env: Environment) extends Unary.Apply(env) {
  val liftedF = func.lift

  override def apply(t: Term) =
    arr(t.label.id) match {
      case f =>
        val processedT = f(t)
        liftedF(processedT).getOrElse(processedT)
    }
}
