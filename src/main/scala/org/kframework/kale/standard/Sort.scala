package org.kframework.kale.standard

import org.kframework.kale

import scala.language.implicitConversions

case class Sort(name: String) extends kale.Sort

object Sort {
  val K = Sort("K")
  val Top = K
  val Bottom = Sort("KBottom")
}
