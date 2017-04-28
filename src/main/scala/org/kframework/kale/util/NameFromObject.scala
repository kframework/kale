package org.kframework.kale.util

trait NameFromObject {
  val name: String = this.getClass.getName.drop(5)
}
