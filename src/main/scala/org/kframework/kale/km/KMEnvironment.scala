package org.kframework.kale.km

import org.kframework.kale._
import org.kframework.kale.standard.{Bottomize, DNFEnvironment}
import org.kframework.minikore.interfaces.pattern
import org.kframework.kale.standard

class KMEnvironment extends DNFEnvironment with Bottomize {
  implicit val env = this

  override val Variable: VariableLabel = standard.SimpleVariableLabel()
  override val Rewrite: RewriteLabel = ???
  override val Equality: EqualityLabel = ???
  override val Truth: TruthLabel = ???
  override val Bottom: Truth with pattern.Bottom = ???
  override val Top: Truth with Substitution with pattern.Top = ???
}
