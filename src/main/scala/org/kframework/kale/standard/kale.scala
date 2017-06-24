package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._

import scala.language.implicitConversions

case class Sort(name: String) extends kale.Sort

object Sort {
  object K extends Sort("K")
}
case class SimpleFreeLabel0 private(name: String)(implicit val env: Environment) extends FreeLabel0

case class SimpleFreeLabel1 private(name: String)(implicit val env: Environment) extends FreeLabel1

case class SimpleFreeLabel2 private(name: String)(implicit val env: Environment) extends FreeLabel2

case class SimpleFreeLabel3 private(name: String)(implicit val env: Environment) extends FreeLabel3

case class SimpleFreeLabel4 private(name: String)(implicit val env: Environment) extends FreeLabel4
