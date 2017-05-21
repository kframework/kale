package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.util.Util

trait DNFEnvironment extends Environment {
  override val Truth: TruthLabel = standard.StandardTruthLabel()

  override val Top: Top = standard.TopInstance()
  override val Bottom: Bottom = standard.BottomInstance()

  override val And: DNFAndLabel = DNFAndLabel()
  override val Or: DNFOrLabel = DNFOrLabel()
  override val Not: NotLabel = NotLabel()
  override val Variable: StandardVariableLabel = standard.StandardVariableLabel()
  override val Equality: EqualityLabel = standard.StandardEqualityLabel()

  override val Next: NextLabel = SimpleNextLabel()

  override val Rewrite = StandardRewriteLabel()

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(Name(v.name + "!" + Math.random().toInt), v.sort)))).toMap)
    rename(t).asInstanceOf[T]
  }
}
