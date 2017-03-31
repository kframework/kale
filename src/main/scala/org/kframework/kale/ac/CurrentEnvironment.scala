package org.kframework.kale.ac

import org.kframework.kale._
import org.kframework.kale.context.{AnywhereContextApplicationLabel, PatternContextApplicationLabel}
import org.kframework.kale.util.Util
import org.kframework.minikore.interfaces.pattern

/**
  * Created by cos on 3/31/17.
  */
class CurrentEnvironment extends Environment {
  implicit private val tthis = this

  import ac._

  val Variable = free.SimpleVariableLabel()

  val Truth = free.SimpleTruthLabel()

  val Hole = Variable("☐")

  val Top: Top = free.TopInstance()
  val Bottom: Bottom = free.BottomInstance()

  val Equality = SimpleEqualityLabel()
  val And = DNFAndLabel()
  val Or = DNFOrLabel()
  val Rewrite = SimpleRewriteLabel()

  val AnywhereContext = AnywhereContextApplicationLabel()
  val CAPP = PatternContextApplicationLabel("CAPP")

  val builtin = new Builtins()(this)

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

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(v.name + Math.random().toInt)))).toMap)
    rename(t).asInstanceOf[T]
  }
}
