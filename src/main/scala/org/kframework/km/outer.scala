package org.kframework.km

object outer {

  import term._
  import builtin._

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

  case class SimpleRewrite(l: Term, r: Term, c: Term) {
    assert(c.sort == SortBool)
  }

  type Terms = Seq[Term]

}
