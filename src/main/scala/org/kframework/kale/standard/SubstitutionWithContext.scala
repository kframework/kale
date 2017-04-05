package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.context.{Context1ApplicationLabel, Context1ProcessingFunction}

case class SubstitutionWithContext(override val substitution: Substitution)(implicit val env: CurrentEnvironment) extends SubstitutionApply(substitution)(env) {

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case l: Context1ApplicationLabel => new Context1ProcessingFunction()(env)
  }) orElse super.processingFunctions
}
