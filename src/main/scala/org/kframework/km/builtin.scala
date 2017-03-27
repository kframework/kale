package org.kframework.km

import scala.util.control.ControlThrowable

object builtin {

  import term._

  case class INT(v: Int) extends Constant {
    val sort: Sort = SortInt
  }

  case class BOOL(v: Boolean) extends Constant {
    val sort: Sort = SortBool
  }

  /* TODO: support string, real, float, and bit-vector
  case class STRING(v: String) extends Constant {
    val sort: Sort = SortString
  }

  case class REAL(v: Double) extends Constant {
    val sort: Sort = SortReal
  }

  case class MINT(v: BitVector) extends Constant {
    val sort: Sort = SortMInt
  }
   */

  object INT {
    trait bop extends Symbol {
      def f(i1: Int, i2: Int): Int
      override val signature: Type = (Seq(SortInt, SortInt), SortInt)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (INT(i1), INT(i2)) => INT(f(i1,i2))
          case _ => Application(this, children)
        }
      }
    }
    object plus  extends bop { override val name: String = "_+Int_"; override def f(i1:Int, i2:Int): Int = i1 + i2 }
    object minus extends bop { override val name: String = "_-Int_"; override def f(i1:Int, i2:Int): Int = i1 - i2 }
    object mult  extends bop { override val name: String = "_*Int_"; override def f(i1:Int, i2:Int): Int = i1 * i2 }
    object div   extends bop { override val name: String = "_/Int_"; override def f(i1:Int, i2:Int): Int = i1 / i2 }
  }

  object BOOL {
    trait compare extends Symbol {
      def f(i1: Int, i2: Int): Boolean
      override val signature: Type = (Seq(SortInt, SortInt), SortBool)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (INT(i1), INT(i2)) => BOOL(f(i1,i2))
          case _ => Application(this, children)
        }
      }
    }
    object gt extends compare { override val name: String = "_>Int_";  override def f(i1: Int, i2: Int): Boolean = i1 > i2 }
    object lt extends compare { override val name: String = "_<Int_";  override def f(i1: Int, i2: Int): Boolean = i1 < i2 }
    object ge extends compare { override val name: String = "_>=Int_"; override def f(i1: Int, i2: Int): Boolean = i1 >= i2 }
    object le extends compare { override val name: String = "_<=Int_"; override def f(i1: Int, i2: Int): Boolean = i1 <= i2 }

    object eq extends Symbol {
      override val name: String = "_==K_"
      override val signature: Type = (Seq(SortK, SortK), SortBool)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 2)
        val (t1,t2) = (children(0), children(1))
        if (t1 == t2) BOOL(true)
        else Application(this, children)
      }
    }

    trait bop extends Symbol {
      def f(b1: Boolean, b2: Boolean): Boolean
      override val signature: Type = (Seq(SortBool, SortBool), SortBool)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (BOOL(b1), BOOL(b2)) => BOOL(f(b1,b2))
          case _ => Application(this, children)
        }
      }
    }
    object and extends bop { override val name: String = "_andBool_"; override def f(b1:Boolean, b2:Boolean): Boolean = b1 && b2 }
    object or  extends bop { override val name: String = "_orBool_";  override def f(b1:Boolean, b2:Boolean): Boolean = b1 || b2 }

    object not extends Symbol {
      override val name: String = "_notBool_"
      override val signature: Type = (Seq(SortBool), SortBool)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 1)
        children(0) match {
          case BOOL(b) => BOOL(!b)
          case _ => Application(this, children)
        }
      }
    }
  }

  object MAPK {
    case class NotFound() extends ControlThrowable

    object select extends Symbol {
      override val name: String = "selectMapK"
      override val signature: Type = (Seq(SortMapK, SortK), SortK)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 2)
        val default = Application(this, children)
        val (m,k) = (children(0), children(1))
        if (k.isSymbolic) default
        else {
          def _select(m1: Term, k1: Term): Term = {
            m1 match {
              case Application(`store`, Seq(m2, k2, v2)) =>
                if (k1 == k2) v2
                else _select(m2, k1)
              case _ => throw NotFound()
            }
          }
          try {
            _select(m,k)
          } catch {
            case NotFound() => default
          }
        }
      }
    }

    object store extends Symbol {
      override val name: String = "storeMapK"
      override val signature: Type = (Seq(SortMapK, SortK, SortK), SortK)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 3)
        val default = Application(this, children)
        val (m,k,v) = (children(0), children(1), children(2))
        if (k.isSymbolic) default
        else {
          def _store(m1: Term, k1: Term, v1: Term): Term = {
            m1 match {
              case Application(`store`, Seq(m2, k2, v2)) =>
                if (k1 == k2)
                  Application(store, Seq(m2, k2, v1))
                else
                  Application(store, Seq(_store(m2, k1, v1), k2, v2))
              case _ => throw NotFound()
            }
          }
          try {
            _store(m,k,v)
          } catch {
            case NotFound() => default
          }
        }
      }
    }
  }

  object LISTK {
    object nil extends Symbol {
      override val name: String = "nilListK"
      override val signature: Type = (Seq(), SortListK)
      override val isFunctional: Boolean = false
      override def apply(children: Seq[Term]): Term = Application(this, children)
    }

    object insert extends Symbol {
      override val name: String = "insertListK"
      override val signature: Type = (Seq(SortK, SortListK), SortListK)
      override val isFunctional: Boolean = false
      override def apply(children: Seq[Term]): Term = Application(this, children)
    }

    object append extends Symbol {
      override val name: String = "appendListK"
      override val signature: Type = (Seq(SortListK, SortListK), SortListK)
      override val isFunctional: Boolean = true
      override def apply(children: Seq[Term]): Term = {
        assert(children.size == 2)
        val default = Application(this, children)
        val (l1,l2) = (children(0), children(1))
        (l1, l2) match {
          case (_, Application(`nil`, _)) => l1
          case (Application(`nil`, _), _) => l2
          case (Application(`insert`, Seq(l11, l12)), _) =>
            Application(insert, Seq(l11, append(l12, l2)))
          case _ => default
        }
      }
    }
  }

}
