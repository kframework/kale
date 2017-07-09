package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.util.Named

trait MacroMixin {
  self: Environment =>

  trait MacroLabel extends Label {
    // the macros are executed at "compile-time", environment build time, so they are not registered
    override val id: Int = -7
  }

  def Macro1(name: String) = new Named(name) with MacroLabel with Label1 {
    override def apply(_1: Term): Term = ???
  }
}
