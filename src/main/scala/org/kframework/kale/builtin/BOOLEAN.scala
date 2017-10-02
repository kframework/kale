package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.util.Named


trait BooleanMixin extends kale.BooleanMixin {
  _: Environment =>

  override val BOOLEAN = new BOOLEAN {
    val Boolean = new ReferenceLabel[Boolean]("Bool@BOOL-SYNTAX") {
      override protected[this] def internalInterpret(s: String): Boolean = s.toBoolean
    }

    val not = PrimitiveFunction1[Boolean]("notBool_", Boolean, x => !x)
    val and = PrimitiveFunction2[Boolean]("_andBool_", Boolean, (x, y) => x && y)
    val or = PrimitiveFunction2[Boolean]("_orBool_", Boolean, (x, y) => x || y)

    /**
      * ifThenElse(c, t, e) is semantically equivalent to Or(And(c = True, t), And(c = False, t)) but evaluated lazily
      * i.e., the t and e are only touched when we know whether the condition is True or False
      * see also STRATEGY.ifThenElse
      */
    val ifThenElse = new Named("BOOLEAN.ifThenElse") with Label3 {
      override val isPredicate: Option[Boolean] = None

      override def apply(condition: Term, thenTerm: Term, elseTerm: Term): Term = condition match {
        case True => thenTerm
        case False => elseTerm
        case _ => FreeNode3(this, condition, thenTerm, elseTerm)
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
