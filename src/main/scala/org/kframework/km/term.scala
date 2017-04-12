package org.kframework.km

object term {

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
  case class Constructor(name: String, signature: Type) extends Symbol {
    val isFunctional: Boolean = false
    val smt: String = name
    val smtBuiltin: Boolean = false
    def applySeq(children: Seq[Term]): Term = Application(this, children)
  }

  sealed trait Term {
    val sort: Sort
    val isSymbolic: Boolean
    val isFunctional: Boolean
    def subst(m: Substitution): Term
    def rename(cnt: Int) : Term
  }
  case class Application(symbol: Symbol, children: Seq[Term]) extends Term {
    assert(symbol.signature._1 == children.map(_.sort))
    val sort: Sort = symbol.signature._2
    val isSymbolic: Boolean = children.exists(_.isSymbolic)
    val isFunctional: Boolean = symbol.isFunctional
    def subst(m: Substitution): Term = {
      symbol.applySeq(children.map(_.subst(m))) // TODO: return this if no children are changed
    }
    def rename(cnt: Int): Term = {
      symbol.applySeq(children.map(_.rename(cnt)))
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

  type Substitution = Map[Variable, Term]

  type Type = Product2[Seq[Sort], Sort]

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

}
