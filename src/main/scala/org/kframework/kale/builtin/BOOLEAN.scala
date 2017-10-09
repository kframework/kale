package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.util.Named


trait BooleanMixin extends kale.BooleanMixin {
  _: Environment =>

  override val BOOLEAN = new BOOLEAN {
    implicit val Boolean: ReferenceLabel[Boolean] = define[Boolean]("Bool@BOOL-SYNTAX")(_.toBoolean)

    val not = define("notBool_", !(_: Boolean))
    val and = define("_andBool_", (_: Boolean) && (_: Boolean))
    val or = define("_orBool_", (_: Boolean) || (_: Boolean))

    /**
      * ifThenElse(c, t, e) is semantically equivalent to Or(And(c = True, t), And(c = False, t)) but evaluated lazily
      * i.e., the t and e are only touched when we know whether the condition is True or False
      * see also STRATEGY.ifThenElse
      */
    val ifThenElse = new Named("BOOLEAN.ifThenElse") with FunctionLabel3 {
      override val isPredicate: Option[Boolean] = None

      override def f(condition: Term, thenTerm: Term, elseTerm: Term) = condition match {
        case True => Some(thenTerm)
        case False => Some(elseTerm)
        case _ => None
      }
    }

    val isTrue = new Named("isTrue") with FunctionLabel1 {

      override val isPredicate: Option[Boolean] = Some(true)

      override def f(_1: Term): Option[Term] = _1 match {
        case Boolean(true) => Some(Top)
        case Boolean(false) => Some(Bottom)
        case _ => None
      }
    }

    val True = Boolean(true)
    val False = Boolean(false)
  }

  implicit val upBoolean = BOOLEAN.Boolean
}
