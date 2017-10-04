package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale.{DefineMixin, Environment, IntMixin, Mixin, MonoidLabeled, UpDown}

trait ScalaLibraryMixin extends Mixin {
  _: Environment with AssocWithIdListMixin with FreeMixin with DefineMixin with IntMixin =>

  val emptyScalaList = FreeLabel0("emptyScalaList")()
  val scalaList = AssocWithIdLabel("scalaList", emptyScalaList)

  implicit val monoidLabeledList: MonoidLabeled[List] = new MonoidLabeled[List] {
    override def monoidLabel: kale.MonoidLabel = scalaList
  }

  import cats.implicits._

  private implicit val updownInt = INT.Int

  implicit val upDownList = implicitly[UpDown[List[Int]]]
}
