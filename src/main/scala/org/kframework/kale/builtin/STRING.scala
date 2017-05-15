package org.kframework.kale.builtin

import org.kframework.kale.{DomainValue, Environment, Label}
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.builtin

case class STRING()(implicit protected val penv: Environment) extends Module("STRING")   {
  val String = new ReferenceLabel[String]("String")(penv) {
    override protected[this] def internalInterpret(s: String): String = s
  }
  override val all: Set[Label] = Set(String)
}

trait importSTRING {
  protected val env: Environment

  lazy val STRING = builtin.STRING()(env)

  implicit def toSTRING(s: String): DomainValue[String] = STRING.String(s)

}
