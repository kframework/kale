package org.kframework.kale.builtin

import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.util.Named
import org.kframework.kale._

trait HasINT {
  self: Environment =>

  val INT = new ReferenceLabel[Int]("Int")(this) {
    override protected[this] def internalInterpret(s: String): Int = s.toInt
  }

  implicit def toINT(i: Int): DomainValue[Int] = INT(i)
}

trait HasINTbop extends HasINTplus with HasINTminus with HasINTmult with HasINTdiv with HasINTmod { self: Environment => }

trait HasINTplus extends HasINT { self: Environment =>
  val intPlus = new Named("_+Int_")(self) with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(INT(a + b))
      case _ => None
    }
    override def smtName: String = "+"
  }
}

trait HasINTminus extends HasINT { self: Environment =>
  val intMinus = new Named("_-Int_")(self) with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(INT(a + b))
      case _ => None
    }
    override def smtName: String = "-"
  }
}

trait HasINTmult extends HasINT { self: Environment =>
  val intMult = new Named("_*Int_")(self) with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(INT(a * b))
      case _ => None
    }
    override def smtName: String = "*"
  }
}

trait HasINTdiv extends HasINT { self: Environment =>
  val intDiv = new Named("_/Int_")(self) with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
    //case (_, INT(0)) => None
    //case (INT(0), b) if b.isGround => Some(INT(0))
      case (INT(a), INT(b)) => Some(INT(a / b))
      case _ => None
    }
    override def smtName: String = "div" // integer division, while "/" is real division.
  }
}

trait HasINTmod extends HasINT { self: Environment =>
  val intMod = new Named("_%Int_")(self) with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(INT(a % b))
      case _ => None
    }
    override def smtName: String = "mod" // z3 also has "rem", remainder.
  }
}

trait HasINTcmp extends HasINTlt with HasINTle with HasINTgt with HasINTge { self: Environment => }

trait HasINTlt extends HasINT with HasBOOLEAN { self: Environment =>
  val intLt = new Named("_<Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a < b))
      case _ => None
    }
    override def smtName: String = "<"
  }
}

trait HasINTle extends HasINT with HasBOOLEAN { self: Environment =>
  val intLe = new Named("_<=Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a <= b))
      case _ => None
    }
    override def smtName: String = "<="
  }
}

trait HasINTgt extends HasINT with HasBOOLEAN { self: Environment =>
  val intGt = new Named("_>Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a > b))
      case _ => None
    }
    override def smtName: String = ">"
  }
}

trait HasINTge extends HasINT with HasBOOLEAN { self: Environment =>
  val intGe = new Named("_>=Int_")(self) with FunctionLabel2 with Z3Builtin {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (INT(a), INT(b)) => Some(BOOLEAN(a >= b))
      case _ => None
    }
    override def smtName: String = ">="
  }
}
