package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.util.Util

trait DNFEnvironment extends Environment {
  private implicit val env = this

  override val Truth: TruthLabel = standard.SimpleTruthLabel()

  override val Top: Top = standard.TopInstance()
  override val Bottom: Bottom = standard.BottomInstance()

  override val And: DNFAndLabel = DNFAndLabel()
  override val Or: DNFOrLabel = DNFOrLabel()
  override val Not: NotLabel = NotLabel()
  override val Variable: SimpleVariableLabel = standard.SimpleVariableLabel()
  override val Equality: EqualityLabel = standard.SimpleEqualityLabel()

  override val Rewrite = SimpleRewriteLabel()

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(v.name + "!" + Math.random().toInt, v.sort)))).toMap)
    rename(t).asInstanceOf[T]
  }
}
