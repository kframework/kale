package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale.{standard, _}
import standard._
import org.kframework.kale.context.{AnywhereContextApplicationLabel, PatternContextApplicationLabel}
import org.kframework.kale.util.Util

trait Bottomize {
  self: Environment =>

  var bottomizeIsActive = false

  def bottomize(_1: Term)(f: => Term): Term = {
    if (Bottom == _1 && bottomizeIsActive)
      Bottom
    else
      f
  }

  def bottomize(_1: Term, _2: Term)(f: => Term): Term = {
    if (Bottom == _1 || Bottom == _2 && bottomizeIsActive)
      Bottom
    else
      f
  }

  def bottomize(terms: Term*)(f: => Term): Term = {
    if (bottomizeIsActive)
      strongBottomize(terms: _*)(f)
    else
      f
  }

  def strongBottomize(terms: Term*)(f: => Term): Term = {
    if (terms.contains(Bottom))
      Bottom
    else
      f
  }
}

class CurrentEnvironment extends DNFEnvironment with HasBOOLEAN with HasINT with HasINTdiv with HasDOUBLE with HasSTRING with HasID {
  implicit val env = this

  val Hole = Variable("☐", Sort.K)

  val IfThenElse = new IfThenElseLabel()
  val BindMatch = new BindMatchLabel()

  val AnywhereContext = AnywhereContextApplicationLabel()

  val builtin = new Builtins()(this)

  override def sort(l: Label, children: Seq[Term]): kale.Sort = Sort.K

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(v.name + Math.random().toInt, v.sort)))).toMap)
    rename(t).asInstanceOf[T]
  }
}
