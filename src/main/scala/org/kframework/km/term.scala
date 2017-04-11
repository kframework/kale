package org.kframework.km

object term {

  sealed trait Sort {
    val name: String
    def smt: String = name
    val smtBuiltin: Boolean
    override def toString: String = name
  }
  case class SortOf(name: String) extends Sort {
    val smtBuiltin: Boolean = false
  }
  case class SortMap(key: Sort, value: Sort) extends Sort {
    val name: String = "Map{" + key.name + "," + value.name + "}"
    override val smt: String = "(Array " + key.smt + " " + value.smt + ")"
    val smtBuiltin: Boolean = true
  }
  case class SortList(elem: Sort) extends Sort {
    val name: String = "List{" + elem.name + "}"
    override val smt: String = "(List " + elem.smt + ")"
    val smtBuiltin: Boolean = true
  }
  // TODO: prevent duplicate builtin sorts
  val SortBool = new SortOf("Bool") {
    override val smtBuiltin: Boolean = true
  }
  val SortInt = new SortOf("Int") {
    override val smtBuiltin: Boolean = true
  }
  val SortString = new SortOf("String") { // TODO: altenative z3 encoding? (e.g., int)?
    override val smtBuiltin: Boolean = true
  }
  // TODO: SortReal, SortMInt, etc
  //
  val SortK = SortOf("K")
  val SortListK = SortList(SortK)

  type Type = Product2[Seq[Sort], Sort]

  trait Symbol {
    val name: String
    val signature: Type
    val isFunctional: Boolean
    val smt: String
    val smtBuiltin: Boolean
    def apply(children: Term*): Term = applySeq(children.toSeq)
    def applySeq(children: Seq[Term]): Term
    override def toString: String = name
  }

  type Substitution = Map[Variable, Term]

  sealed trait Term {
    val sort: Sort
    val isSymbolic: Boolean
    val isFunctional: Boolean
    def subst(m: Substitution): Term
    def rename(cnt: Int) : Term
  }
  case class Application(symbol: Symbol, children: Seq[Term]) extends Term {
    assert(symbol.signature._1 == children.map(t => t.sort))
    val sort: Sort = symbol.signature._2
    val isSymbolic: Boolean = children.exists(t => t.isSymbolic)
    val isFunctional: Boolean = symbol.isFunctional
    def subst(m: Substitution): Term = {
      symbol.applySeq(children.map(t => t.subst(m))) // TODO: return this if no children are changed
    }
    def rename(cnt: Int): Term = {
      symbol.applySeq(children.map(t => t.rename(cnt)))
    }
    override def toString: String = symbol + "(" + children.map(_.toString).mkString(",") + ")"
  }
  case class Variable(name: String, sort: Sort) extends Term {
    val isSymbolic: Boolean = true
    val isFunctional: Boolean = false
    def subst(m: Substitution): Term = {
      if (m.contains(this)) m(this) else this // TODO: m.getOrElse(this, this)
    }
    def rename(cnt: Int): Term = Variable(name + "!" + cnt, sort)
    override def toString: String = name + ":" + sort.name
  }
  trait Constant extends Term {
    val smt: String
    override val isSymbolic: Boolean = false
    override val isFunctional: Boolean = false
    override def subst(m: Substitution): Term = this
    override def rename(cnt: Int): Term = this
  }

  ////

  case class Constructor(name: String, signature: Type) extends Symbol {
    val isFunctional: Boolean = false
    val smt: String = name
    val smtBuiltin: Boolean = false
    def applySeq(children: Seq[Term]): Term = Application(this, children)
  }

  case class SimplePattern(term: Term, constraint: Term) {
    assert(constraint.sort == SortBool)
    val counter: Int = 0
    override def toString: String = term + " /\\ " + constraint
  }
  object SimplePattern {
    def apply(term: Term, constraint: Term, cnt: Int): SimplePattern = new SimplePattern(term, constraint) {
      override val counter: Int = cnt
    }
  }

  type Terms = Seq[Term]

  case class SimpleRewrite(l: Term, r: Term, c: Term) {
    assert(c.sort == SortBool)
  }

}
