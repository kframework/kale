package org.kframework.km

object term {

  sealed trait Sort
  case class SortOf(name: String) extends Sort
  case class SortMap(key: Sort, value: Sort) extends Sort
  case class SortList(elem: Sort) extends Sort
  object SortBool extends Sort
  object SortInt extends Sort
  //
  object SortK extends SortOf("K")
  object SortMapK extends SortMap(SortK, SortK)
  object SortListK extends SortList(SortK)

  type Type = Product2[Seq[Sort], Sort]

  trait Symbol {
    val name: String
    val signature: Type
    val isFunctional: Boolean
    //
    def apply(children: Seq[Term]): Term
  }

  type Substitution = Map[Variable, Term]

  sealed trait Term {
    val sort: Sort
    def subst(m: Substitution): Term
  }
  case class Variable(name: String, sort: Sort) extends Term {
    def subst(m: Substitution): Term = {
      if (m.contains(this)) m(this) else this // TODO: m.getOrElse(this, this)
    }
  }
  trait Constant extends Term {
    val sort: Sort
    def subst(m: Substitution): Term = this
  }
  case class Application(symbol: Symbol, children: Seq[Term]) extends Term {
    assert(symbol.signature._1.equals(children.map(t => t.sort)))
    val sort: Sort = symbol.signature._2
    def subst(m: Substitution): Term = {
      symbol.apply(children.map(t => t.subst(m))) // TODO: return this if no children are changed
    }
  }

  case class SimplePattern(term: Term, constraint: Term) {
    assert(constraint.sort.equals(SortBool))
  }

  type Terms = Seq[Term]

  case class SimpleRewrite(l: Term, r: Term, c: Term) {
    assert(c.sort.equals(SortBool))
  }

}
