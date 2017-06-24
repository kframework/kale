package org.kframework.kale.builtin

import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

case class STRING()(implicit protected val penv: Environment with hasINT with hasBOOLEAN) {

  import penv._

  val String = new ReferenceLabel[String]("String")(penv) {
    override protected[this] def internalInterpret(s: String): String = s
  }

  val substr = PrimitiveFunction3[String, Int, Int, String]("substrString", String, INT.Int, INT.Int, String, (a, b, c) => a.substring(b, c))

  //Todo: From what I understand the hooks do.
  val findstr = PrimitiveFunction3[String, String, Int, Int]("findString", String, String, INT.Int, INT.Int, (a, b, c) => a.indexOf(b, c))

  val rfindstr = PrimitiveFunction3[String, String, Int, Int]("rfindString", String, String, INT.Int, INT.Int, (a, b, c) => a.lastIndexOf(b, c))

  val findchar = PrimitiveFunction3[String, String, Int, Int]("findChar", String, String, INT.Int, INT.Int, (a, b, c) => a.indexOf(b.charAt(0), c))

  val rfindchar = PrimitiveFunction3[String, String, Int, Int]("rfindChar", String, String, INT.Int, INT.Int, (a, b, c) => a.lastIndexOf(b.charAt(0), c))

  val strconcat = PrimitiveFunction2[String, String]("+String", String, String, (x, y) => x.concat(y))

  val replaceall = PrimitiveFunction3[String]("replaceAll(_,_,_)", String, (a, b, c) => a.replaceAll(b, c))

  val replacefirst = PrimitiveFunction3[String]("replaceFirst(_,_,_)", String, (a, b, c) => a.replaceFirst(b, c))

  val stringne = PrimitiveFunction2[String, String, Boolean]("_=/=String_", String, String, BOOLEAN.Boolean , (x, y) => x != y)

  val stringe = PrimitiveFunction2[String, String, Boolean]("_==String_", String, String, BOOLEAN.Boolean , (x, y) => x == y)
}

trait importSTRING {
  protected val env: Environment with hasINT with hasBOOLEAN

  val STRING = builtin.STRING()(env)

  implicit def toSTRING(s: String): DomainValue[String] = STRING.String(s)

}
