package org.kframework.kale.util

import org.kframework.kale._
import org.kframework.kale.builtin.importINT
import org.kframework.kale.standard.{SimpleFreeLabel2, StandardEnvironment}

import scala.language.implicitConversions

class Implicits(implicit val env: StandardEnvironment) extends StaticImplicits {

  import env._

  val plus = SimpleFreeLabel2("+")

  //PrimitiveFunction2("+", INT, (a: Int, b: Int) => a + b)(env)

  implicit class asTerm(x: Term) {
    def +(y: Term): Term = plus(x, y)
  }

  implicit class RichTerm(t: Term) {
    def :=(tt: Term)(implicit m: MatcherOrUnifier): Term = m(t, tt)
    def =:=(tt: Term)(implicit m: MatcherOrUnifier): Term = m(t, tt)
  }

}

trait StaticImplicits {

  implicit class StaticRichAssocLabel(label: AssocLabel) {
    def apply(args: Term*): Term = label.apply(args.toSeq)
  }

  implicit def symbolWithApp(s: Symbol)(env: Environment) = new {
    val label = env.label(s.name)

    def apply[T](value: T): Term = label.asInstanceOf[LeafLabel[T]](value)

    def apply(_1: Term): Term = label.asInstanceOf[Label1](_1)

    def apply(_1: Term, _2: Term): Term = label.asInstanceOf[Label2](_1, _2)
  }

  implicit def PFtoTotal(f: PartialFunction[Term, Boolean]): (Term => Boolean) = x => f.lift(x).getOrElse(false)

  implicit class StaticRichTerm(t: Term) {
    def contains(subterm: Term): Boolean = Util.contains(t, subterm)

    def find(f: Term => Boolean): Option[Term] = {
      if (f(t))
        Some(t)
      else t match {
        case Node(l, children) => children.toStream flatMap (_ find f) headOption
        case _ => None
      }
    }

    def exists(f: Term => Boolean): Boolean = find(f).isDefined

    // if (t == subterm) true else t.children.exists(_.contains(subterm))
    def containsInConstructor(subterm: Term): Boolean = Util.containsInConstructor(t, subterm)
  }
}

object StaticImplicits extends StaticImplicits