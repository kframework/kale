package org.kframework.kale.builtin

import cats.Monoid
import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply


trait StringMixin extends kale.StringMixin {
  _: Environment with IntMixin with BooleanMixin with Mixin =>

  override val STRING = new STRING {

    implicit val String = define[String]("String")(x => x)

    val Regex = define[scala.util.matching.Regex]("Regex")(_.r)

    val substr = define("substrString", (_: String).substring(_: Int, _: Int))

    //Todo: From what I understand the hooks do.
    val findstr = define("findString", (_: String).indexOf(_: String, _: Int))

    val rfindstr = define("rfindString", (_: String).lastIndexOf(_: String, _: Int))

    val findchar = define("findChar", (_: String).indexOf((_: String).charAt(0), _: Int))

    val rfindchar = define("rfindChar", (_: String).lastIndexOf((_: String).charAt(0), _: Int))

    implicit val concatMonoid = new Monoid[String] {
      override def empty = ""

      override def combine(x: String, y: String) = x concat y
    }

    val strconcat = monoid[String]("_+String_")

    val replaceall = define("replaceAll(_,_,_)", (_: String).replaceAll(_: String, _: String))

    val replacefirst = define("replaceFirst(_,_,_)", (_: String).replaceFirst(_: String, _: String))

    val stringne = define("_=/=String_", (_: String) != (_: String))

    val stringe = define("_==String_", (_: String) == (_: String))
  }

  import STRING._

  implicit val upString = String

  case class RegexMatch(solver: Apply) extends Binary.F({ (r: DomainValue[scala.util.matching.Regex], s: Term) =>
    val reg = r.data
    s match {
      case String(reg()) => s
      case _ => Bottom
    }
  })

  register(Binary.definePartialFunction({
    case (Regex, String) => RegexMatch
  }), Priority.low + 1)
}
