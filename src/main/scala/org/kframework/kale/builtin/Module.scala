package org.kframework.kale.builtin

import org.kframework.kale.{Environment, Label, util}

abstract class Module(val moduleName: String)(implicit val env: Environment) {

  private def fullname(localName: String): String = localName + "@" + moduleName

  protected abstract class LocalName[E <: Environment](val localName: String) extends util.Named(fullname(localName))

  val all: Set[Label]

  private lazy val labelName2Label = all.map(l => (l.name, l)).toMap

  def apply(localName: String): Label = labelName2Label(fullname(localName))
}
