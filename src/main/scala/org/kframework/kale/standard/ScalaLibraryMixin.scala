package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale.{Environment, Mixin, MonoidLabeled, Term, Up}

trait ScalaLibraryMixin extends Mixin {
  env: Environment with AssocWithIdListMixin with FreeMixin =>

  val emptyScalaList = FreeLabel0("emptyScalaList")()
  val scalaList = AssocWithIdLabel("scalaList", emptyScalaList)

  final def upScalaList[T: Up] = new Up[Iterable[T]] {
    override def apply(l: Iterable[T]): Term = scalaList(l map implicitly[Up[T]])
  }

  implicit val monoidLabeled: MonoidLabeled[List] = new MonoidLabeled[List] {
    override def monoidLabel: kale.MonoidLabel = scalaList
  }
}
