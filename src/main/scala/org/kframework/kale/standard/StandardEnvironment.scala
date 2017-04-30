package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale.builtin._
import org.kframework.kale.context.anywhere.AnywhereContextApplicationLabel
import org.kframework.kale.{standard, _}

object StandardEnvironment {
  def apply(): StandardEnvironment = new StandardEnvironment {}
}

trait StandardEnvironment extends DNFEnvironment with HasBOOLEAN with HasINT with HasINTdiv with HasDOUBLE with HasSTRING with HasID {
  private implicit val env = this

  val Hole = Variable("☐", Sort.K)

  val IfThenElse = new IfThenElseLabel()
  val BindMatch = new BindMatchLabel()

  val AnywhereContext = AnywhereContextApplicationLabel()

  override def sort(l: Label, children: Seq[Term]): kale.Sort = Sort.K

  override val substitutionMaker: (Substitution) => SubstitutionApply = new SubstitutionWithContext(_)
}
