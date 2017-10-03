package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale.{Environment, Mixin, MonoidLabeled, Term, Up}

trait ScalaLibraryMixin extends Mixin {
  _: Environment with AssocWithIdListMixin with FreeMixin =>

  val emptyScalaList = FreeLabel0("emptyScalaList")()
  val scalaList = AssocWithIdLabel("scalaList", emptyScalaList)

  implicit val monoidLabeled: MonoidLabeled[List] = new MonoidLabeled[List] {
    override def monoidLabel: kale.MonoidLabel = scalaList
  }
}
