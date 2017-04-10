package org.kframework.kale.standard

import org.kframework.kale._

sealed trait PrimordialConstantLabel[T] extends ConstantLabel[T] {
  def apply(v: T): Constant[T] = SimpleConstant(this, v)
}

abstract class ReferenceLabel[T](val name: String)(val env: Environment) extends PrimordialConstantLabel[T]

class Builtins(implicit val env: CurrentEnvironment) {

  val eenv = env


  case class Sort(name: String)

  case class GENERIC_TOKEN(sort: Sort) extends {
    val name = "TOKEN_" + sort.name
  } with env.HasEnvironment with PrimordialConstantLabel[String] {
    override def internalInterpret(s: String): String = s
  }

  val BuiltinSetUnit = FreeLabel0(".Set")
  val BuiltinSet = SetLabel("_Set_", BuiltinSetUnit())
}