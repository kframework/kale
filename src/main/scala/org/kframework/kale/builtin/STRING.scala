package org.kframework.kale.builtin

import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

case class STRING()(implicit protected val penv: Environment with importINT) extends Module("STRING")   {
  import penv._

  val String = new ReferenceLabel[String]("String")(penv) {
    override protected[this] def internalInterpret(s: String): String = s
  }

  val substr = PrimitiveFunction3[String, Int, Int, String]("substrString", String, INT.Int, INT.Int, String, (a, b, c) => a.substring(b, c))

  override val all: Set[Label] = Set(String, substr)
}

trait importSTRING {
  protected val env: Environment with importINT
  val INT: builtin.INT

  val STRING = builtin.STRING()(env)

  implicit def toSTRING(s: String): DomainValue[String] = STRING.String(s)

}
