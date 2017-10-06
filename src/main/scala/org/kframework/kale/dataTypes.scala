package org.kframework.kale

import org.kframework.kale.builtin._

trait IntMixin extends Mixin {
  env: Environment =>

  trait INT {
    val Int: DomainValueLabel[scala.Int]
    val plus: PrimitiveFunction2[scala.Int, scala.Int, scala.Int]
    val minus: PrimitiveFunction2[scala.Int, scala.Int, scala.Int]
    val mult: PrimitiveFunction2[scala.Int, scala.Int, scala.Int]
    val div: PrimitiveFunction2[scala.Int, scala.Int, scala.Int]
    val mod: PrimitiveFunction2[scala.Int, scala.Int, scala.Int]
    val lt: PrimitiveFunction2[scala.Int, scala.Int, scala.Boolean]
    val le: PrimitiveFunction2[scala.Int, scala.Int, scala.Boolean]
    val gt: PrimitiveFunction2[scala.Int, scala.Int, scala.Boolean]
    val ge: PrimitiveFunction2[scala.Int, scala.Int, scala.Boolean]
    val neq: PrimitiveFunction2[scala.Int, scala.Int, scala.Boolean]
    val eq: PrimitiveFunction2[scala.Int, scala.Int, scala.Boolean]

    val all: Set[Label]
  }

  val INT: INT
}

trait DoubleMixin extends Mixin {
  env: Environment =>
  val DOUBLE: {
    val Double: DomainValueLabel[scala.Double]
  }
}

trait BooleanMixin extends Mixin {
  env: Environment =>

  trait BOOLEAN {
    implicit val Boolean: DomainValueLabel[scala.Boolean]
    val not: PrimitiveFunction1[scala.Boolean, scala.Boolean]
    val True: DomainValue[Boolean]
    val False: DomainValue[Boolean]
    val isTrue: FunctionLabel1
  }

  val BOOLEAN: BOOLEAN
}

trait StringMixin extends Mixin {
  env: Environment =>

  trait STRING {
    val String: DomainValueLabel[java.lang.String]
    val Regex: DomainValueLabel[scala.util.matching.Regex]
    val strconcat: PrimitiveMonoid[String]
  }

  val STRING: STRING
}

trait IdMixin extends Mixin {
  env: Environment =>
  val ID: {
    val Id: DomainValueLabel[scala.Symbol]
  }
}