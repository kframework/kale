package org.kframework.kale.standard

import org.kframework.kale.{standard, _}
import standard._
import org.kframework.kale.context.{AnywhereContextApplicationLabel, PatternContextApplicationLabel}
import org.kframework.kale.util.Util

trait Bottomize {
  self: Environment =>

  def bottomize(_1: Term)(f: => Term): Term = {
    if (Bottom == _1)
      Bottom
    else
      f
  }

  def bottomize(_1: Term, _2: Term)(f: => Term): Term = {
    if (Bottom == _1 || Bottom == _2)
      Bottom
    else
      f
  }

  def bottomize(terms: Term*)(f: => Term): Term = {
    if (terms.contains(Bottom))
      Bottom
    else
      f
  }
}

class CurrentEnvironment extends DNFEnvironment {
  implicit val env = this

  val Variable = standard.SimpleVariableLabel()

  val Truth = standard.SimpleTruthLabel()

  val Hole = Variable("☐", Sort.K)

  val Top: Top = standard.TopInstance()
  val Bottom: Bottom = standard.BottomInstance()

  val Equality = SimpleEqualityLabel()
  val Rewrite = SimpleRewriteLabel()

  val AnywhereContext = AnywhereContextApplicationLabel()
  val CAPP = PatternContextApplicationLabel("CAPP")

  val builtin = new Builtins()(this)

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(v.name + Math.random().toInt, v.sort)))).toMap)
    rename(t).asInstanceOf[T]
  }
}
