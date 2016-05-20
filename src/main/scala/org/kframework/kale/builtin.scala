package org.kframework.kale

object INT extends ConstantLabel[Int] {
  object + extends Label2 with UniqueId {
    val name = "+"
  }
  case class +(_1: Term, _2: Term) extends Node2 {
    val label = INT.+
  }
}
