package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale.builtin._
import org.kframework.kale.{standard, _}
import org.kframework.kale.context.AnywhereContextApplicationLabel
import org.kframework.kale.util.Util



class CurrentEnvironment extends DNFEnvironment with HasBOOLEAN with HasINT with HasINTdiv with HasDOUBLE with HasSTRING with HasID {
  implicit val env = this

  val Hole = Variable("☐", Sort.K)

  val IfThenElse = new IfThenElseLabel()
  val BindMatch = new BindMatchLabel()

  val AnywhereContext = AnywhereContextApplicationLabel()

  override def sort(l: Label, children: Seq[Term]): kale.Sort = Sort.K

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(v.name + Math.random().toInt, v.sort)))).toMap)
    rename(t).asInstanceOf[T]
  }
}
