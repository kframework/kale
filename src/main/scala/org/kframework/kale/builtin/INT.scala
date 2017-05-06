package org.kframework.kale.builtin

import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.util.Named
import org.kframework.kale.{Environment, FunctionLabel2, Term, Z3Builtin}

trait HasINT {
  self: Environment =>

  val INT = new ReferenceLabel[Int]("Int")(this) {
    override protected[this] def internalInterpret(s: String): Int = s.toInt
  }
}

trait HasINTdiv extends HasINT {
  self: Environment =>

  val intDiv = new Named("_/Int_")(self) with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (_, INT(0)) => None
      case (INT(0), b) if b.isGround => Some(INT(0))
      case (INT(a), INT(b)) => Some(INT(a / b))
      case _ => None
    }
  }
}

trait HasINTlt extends HasINT with HasBOOLEAN { self: Environment =>
  val intLt = new Named("_<Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a < b))
      case _ => None
    }
    override def smt: String = "<"
  }
}

trait HasINTle extends HasINT with HasBOOLEAN { self: Environment =>
  val intLe = new Named("_<=Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a <= b))
      case _ => None
    }
    override def smt: String = "<="
  }
}

trait HasINTgt extends HasINT with HasBOOLEAN { self: Environment =>
  val intGt = new Named("_>Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a > b))
      case _ => None
    }
    override def smt: String = ">"
  }
}

trait HasINTge extends HasINT with HasBOOLEAN { self: Environment =>
  val intGe = new Named("_>=Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a >= b))
      case _ => None
    }
    override def smt: String = ">="
  }
}
