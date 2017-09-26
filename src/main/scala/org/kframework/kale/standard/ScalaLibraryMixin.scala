package org.kframework.kale.standard

import org.kframework.kale.{Environment, Mixin, Term, Up}

trait ScalaLibraryMixin extends Mixin {
  env: Environment with AssocWithIdListMixin with FreeMixin =>
  val emptyScalaList = FreeLabel0("emptyScalaList")()
  val scalaList = AssocWithIdLabel("scalaList", emptyScalaList)

  final def upScalaList[T: Up] = new Up[List[T]] {
    override def apply(l: List[T]): Term = scalaList(l map implicitly[Up[T]])
  }
}
