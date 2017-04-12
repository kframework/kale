package org.kframework.km

import scala.util.control.ControlThrowable

object builtin {

  import term._

  // sorts

  // TODO: prevent duplicate builtin sorts
  val SortInt = new SortOf("Int") {
    override val smtBuiltin: Boolean = true
  }
  val SortBool = new SortOf("Bool") {
    override val smtBuiltin: Boolean = true
  }
  val SortString = new SortOf("String") { // TODO: altenative z3 encoding? (e.g., int)?
    override val smtBuiltin: Boolean = true
  }
  // TODO: SortReal, SortMInt, etc

  val SortK = SortOf("K")
  val SortListK = SortList(SortK)

  // terms

  case class INT(v: Int) extends Constant {
    val sort: Sort = SortInt
    val smt: String = v.toString
  }
  case class BOOL(v: Boolean) extends Constant {
    val sort: Sort = SortBool
    val smt: String = v.toString
  }
  case class STRING(v: String) extends Constant {
    val sort: Sort = SortString
    val smt: String = "\"" + v + "\""
  }
  /* TODO: support string, real, float, and bit-vector
  case class REAL(v: Double) extends Constant {
    val sort: Sort = SortReal
  }
  case class MINT(v: BitVector) extends Constant {
    val sort: Sort = SortMInt
  }
   */

  // symbols

  object INT {
    sealed trait bop extends Symbol {
      def f(i1: Int, i2: Int): Int
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(SortInt, SortInt), SortInt)
      override val isFunctional: Boolean = true
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (INT(i1), INT(i2)) => INT(f(i1,i2))
          case _ => Application(this, children)
        }
      }
    }
    object plus  extends bop { override val name: String = "_+Int_"; override val smt: String = "+";   override def f(i1:Int, i2:Int): Int = i1 + i2 }
    object minus extends bop { override val name: String = "_-Int_"; override val smt: String = "-";   override def f(i1:Int, i2:Int): Int = i1 - i2 }
    object mult  extends bop { override val name: String = "_*Int_"; override val smt: String = "*";   override def f(i1:Int, i2:Int): Int = i1 * i2 }
    object div   extends bop { override val name: String = "_/Int_"; override val smt: String = "div"; override def f(i1:Int, i2:Int): Int = i1 / i2 }
    object mod   extends bop { override val name: String = "_%Int_"; override val smt: String = "mod"; override def f(i1:Int, i2:Int): Int = i1 % i2 }

    sealed trait cmp extends Symbol {
      def f(i1: Int, i2: Int): Boolean
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(SortInt, SortInt), SortBool)
      override val isFunctional: Boolean = true
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (INT(i1), INT(i2)) => BOOL(f(i1,i2))
          case _ => Application(this, children)
        }
      }
    }
    object gt extends cmp { override val name: String = "_>Int_";  override val smt: String = ">";  override def f(i1: Int, i2: Int): Boolean = i1 > i2 }
    object lt extends cmp { override val name: String = "_<Int_";  override val smt: String = "<";  override def f(i1: Int, i2: Int): Boolean = i1 < i2 }
    object ge extends cmp { override val name: String = "_>=Int_"; override val smt: String = ">="; override def f(i1: Int, i2: Int): Boolean = i1 >= i2 }
    object le extends cmp { override val name: String = "_<=Int_"; override val smt: String = "<="; override def f(i1: Int, i2: Int): Boolean = i1 <= i2 }
  }

  object BOOL {
    sealed trait bop extends Symbol {
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(SortBool, SortBool), SortBool)
      override val isFunctional: Boolean = true
    }
    object and extends bop {
      override val name: String = "_andBool_"
      override val smt: String = "and"
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (BOOL(b1), BOOL(b2)) => BOOL(b1 && b2)
          case (BOOL(true), t) => t
          case (t, BOOL(true)) => t
          case (BOOL(false), _) => BOOL(false)
          case (_, BOOL(false)) => BOOL(false)
          case _ => Application(this, children)
        }
      }
    }
    object or extends bop {
      override val name: String = "_orBool_"
      override val smt: String = "or"
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (BOOL(b1), BOOL(b2)) => BOOL(b1 || b2)
          case (BOOL(true), _) => BOOL(true)
          case (_, BOOL(true)) => BOOL(true)
          case (BOOL(false), t) => t
          case (t, BOOL(false)) => t
          case _ => Application(this, children)
        }
      }
    }
    object implies extends bop {
      override val name: String = "_impliesBool_"
      override val smt: String = "implies"
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        (children(0), children(1)) match {
          case (BOOL(b1), BOOL(b2)) => BOOL(!b1 || b2)
          case (BOOL(true), t) => t
          case (_, BOOL(true)) => BOOL(true)
          case (BOOL(false), t) => BOOL(true)
          case (t, BOOL(false)) => not(t)
          case _ => Application(this, children)
        }
      }
    }
    object not extends Symbol {
      override val name: String = "_notBool_"
      override val smt: String = "not"
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(SortBool), SortBool)
      override val isFunctional: Boolean = true
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 1)
        children(0) match {
          case BOOL(b) => BOOL(!b)
          case _ => Application(this, children)
        }
      }
    }
    case class eq(sort: Sort) extends Symbol {
      override val name: String = "_==" + sort.name + "_"
      override val smt: String = "="
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(sort, sort), SortBool)
      override val isFunctional: Boolean = true
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        val (t1,t2) = (children(0), children(1))
        if (t1 == t2) BOOL(true)
        else Application(this, children)
      }
    }
  }

  /*
     general Map[K,K] doesn't work.
     suppose we have a symbolic map M, and we lookup a key X, which returns M[X].
     now the expression M[X] is of sort K, and can be matched any kind of statements on the top of the <k> cell.
   */
  object MAP {
    case class NotFound() extends ControlThrowable

    /* // TODO: not thread safe
    private val selects: mutable.Map[SortMap, Symbol] = mutable.Map()
    def selectOf(sort: SortMap): Symbol = {
      if (selects.contains(sort)) selects(sort)
      else {
        val symbol = Select(sort)
        selects.put(sort, symbol)
        symbol
      }
    }
    private */
    case class select(sort: SortMap) extends Symbol {
      override val name: String = "selectMap" + sort.key + sort.value
      override val smt: String = "select"
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(sort, sort.key), sort.value)
      override val isFunctional: Boolean = true
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        lazy val default = Application(this, children)
        val (m,k) = (children(0), children(1))
        if (k.isSymbolic) default
        else {
          def select(m1: Term, k1: Term): Term = {
            m1 match {
              case Application(st:store, Seq(m2, k2, v2)) =>
                assert(st.sort == sort)
                if (k1 == k2) v2
                else select(m2, k1)
              case _ => throw NotFound()
            }
          }
          try {
            select(m,k)
          } catch {
            case NotFound() => default
          }
        }
      }
    }

    /* // TODO: not thread safe
    private val stores: mutable.Map[SortMap, Symbol] = mutable.Map()
    def storeOf(sort: SortMap): Symbol = {
      if (stores.contains(sort)) stores(sort)
      else {
        val symbol = Store(sort)
        stores.put(sort, symbol)
        symbol
      }
    }
    private */
    case class store(sort: SortMap) extends Symbol {
      override val name: String = "storeMap" + sort.key + sort.value
      override val smt: String = "store"
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(sort, sort.key, sort.value), sort)
      override val isFunctional: Boolean = true
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 3)
        lazy val default = Application(this, children)
        val (m,k,v) = (children(0), children(1), children(2))
        if (k.isSymbolic) default
        else {
          def store(m1: Term, k1: Term, v1: Term): Term = {
            m1 match {
              case Application(st:store, Seq(m2, k2, v2)) =>
                assert(st.sort == sort)
                if (k1 == k2)
                  Application(this, Seq(m2, k2, v1))
                else
                  Application(this, Seq(store(m2, k1, v1), k2, v2))
              case _ => throw NotFound()
            }
          }
          try {
            store(m,k,v)
          } catch {
            case NotFound() => default
          }
        }
      }
    }
  }

  object LIST {
    case class nil(sort: SortList) extends Symbol {
      override val name: String = "nilList" + sort.elem
      override val smt: String = "nil"
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(), sort)
      override val isFunctional: Boolean = false
      override def applySeq(children: Seq[Term]): Term = Application(this, children)
    }

    case class insert(sort: SortList) extends Symbol {
      override val name: String = "insertList" + sort.elem
      override val smt: String = "insert"
      override val smtBuiltin: Boolean = true
      override val signature: Type = (Seq(sort.elem, sort), sort)
      override val isFunctional: Boolean = false
      override def applySeq(children: Seq[Term]): Term = Application(this, children)
    }

    case class append(sort: SortList) extends Symbol {
      override val name: String = "appendList" + sort.elem
      override val smt: String = "append"
      override val smtBuiltin: Boolean = false
      override val signature: Type = (Seq(sort, sort), sort)
      override val isFunctional: Boolean = true
      override def applySeq(children: Seq[Term]): Term = {
        assert(children.size == 2)
        lazy val default = Application(this, children)
        val (l1,l2) = (children(0), children(1))
        (l1, l2) match {
          case (_, Application(n:nil, _)) => assert(n.sort == sort); l1
          case (Application(n:nil, _), _) => assert(n.sort == sort); l2
          case (Application(ins:insert, Seq(l11, l12)), _) =>
            assert(ins.sort == sort)
            Application(ins, Seq(l11, this(l12, l2)))
          case _ => default
        }
      }
    }
  }

}
