package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.context._
import org.kframework.minikore.interfaces.pattern.Pattern

case class SubstitutionWithContext(override val substitution: Substitution)(implicit val env: CurrentEnvironment) extends SubstitutionApply(substitution)(env) {

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case l: AnywhereContextApplicationLabel => new Context1ProcessingFunction()(env)
    case l: PatternContextApplicationLabel => new PatternContextProcessingFunction()(env)
  }) orElse super.processingFunctions
}
