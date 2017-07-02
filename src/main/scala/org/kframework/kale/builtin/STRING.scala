package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply


case class STRING()(implicit protected val penv: Environment with IntMixin with BooleanMixin) {

  import penv._

  val String = new ReferenceLabel[String]("String")(penv) {
    override protected[this] def internalInterpret(s: String): String = s
  }

  val Regex = new ReferenceLabel[scala.util.matching.Regex]("Regex")(penv) {
    override protected[this] def internalInterpret(s: String): scala.util.matching.Regex = s.r
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

  val stringne = PrimitiveFunction2[String, String, Boolean]("_=/=String_", String, String, BOOLEAN.Boolean, (x, y) => x != y)

  val stringe = PrimitiveFunction2[String, String, Boolean]("_==String_", String, String, BOOLEAN.Boolean, (x, y) => x == y)

}

trait StringMixin extends kale.StringMixin with Environment with IntMixin with BooleanMixin with Mixin with HasMatcher {

  val STRING = builtin.STRING()

  implicit def toSTRING(s: String): DomainValue[String] = STRING.String(s)

  import STRING._

  case class RegexMatch(solver: Apply) extends Binary.F({ (r: DomainValue[scala.util.matching.Regex], s: Term) =>
    val reg = r.data
    s match {
      case String(reg()) => Next(s)
      case _ => Bottom
    }
  })

  override protected def makeMatcher = Binary.definePartialFunction({
    case (Regex, String) => RegexMatch
  }).orElse(super.makeMatcher)
}
