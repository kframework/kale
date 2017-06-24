package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.context.BundledContextMixin
import org.kframework.kale.context.anywhere.{AnywhereContextApplicationLabel, AnywhereContextProcessingFunction}
import org.kframework.kale.context.pattern.{PatternContextApplicationLabel, PatternContextProcessingFunction}

case class SubstitutionWithContext(override val substitution: Substitution)(implicit env: Environment with BundledContextMixin) extends SubstitutionApply(substitution)(env) {

  override def processingFunctions: ProcessingFunctions = definePartialFunction[Term, this.type]({
    case l: AnywhereContextApplicationLabel => new AnywhereContextProcessingFunction()(env)
    case l: PatternContextApplicationLabel => new PatternContextProcessingFunction()(env)
  }) orElse super.processingFunctions
}
