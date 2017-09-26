package org.kframework.kale

import org.kframework.kale.standard.AssocWithIdListMixin

trait ScalaTermsMixin extends Mixin {
  env: Environment with AssocWithIdListMixin with FreeMixin =>
  val emtpyScalaList = FreeLabel0("emptyScalaList")()
  val scalaList = AssocWithIdLabel("scalaList", emtpyScalaList)
}
