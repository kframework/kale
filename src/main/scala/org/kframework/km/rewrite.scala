package org.kframework.km

import scala.collection.mutable

class rewrite(val symbols: Seq[Seq[term.Symbol]]) {

  import term._
  import unification._
  import builtin._
  import outer._

  val z3 = new z3(symbols)

  def applyRule(rule: SimpleRewrite, term: SimplePattern): Seq[SimplePattern] = { val cntRename = term.counter + 1
    // rule:  l => r if c
    lazy val l = rule.l.rename(cntRename)
    lazy val r = rule.r.rename(cntRename)
    lazy val c = rule.c.rename(cntRename)
    // term:  t /\ p
    val t = term.term
    val p = term.constraint

    unify(l,t) match {
      case None => Seq()
      case Some(u) =>
        val _p = p.subst(u.subst)
        val _c = c.subst(u.subst)
        val _p_c_u = BOOL.and(BOOL.and(_p, _c), and(u.constraint))

        if (z3.sat(_p_c_u)) {
          val _r = r.subst(u.subst)
          Seq(SimplePattern(_r, _p_c_u, cntRename))
        } else {
          Seq()
        }
    }
  }

  def applyRules(rules: Seq[SimpleRewrite], term: SimplePattern): Seq[SimplePattern] = {
    rules.flatMap(rule => applyRule(rule, term))
  }

  def searchDepth(depth: Int)(rules: Seq[SimpleRewrite], term: SimplePattern): Seq[SimplePattern] = {
    def loop(depth: Int, currTerms: Seq[SimplePattern], normalTerms: Seq[SimplePattern]): (Seq[SimplePattern], Seq[SimplePattern]) = {
      if (depth == 0 || currTerms.isEmpty) (currTerms, normalTerms)
      else {
        // TODO: more efficient and flexible way?
        case class Next(terms: Seq[SimplePattern])
        case class Done(term: SimplePattern)
        val nextTerms = currTerms.map(t => {
          applyRules(rules,t) match {
            case Seq() => Done(t)
            case ts => Next(ts)
          }
        })
        val (newCurrTerms, newNormalTerms) = nextTerms.partition(_.isInstanceOf[Next])
        val _newCurrTerms = newCurrTerms.flatMap({case Next(ts) => ts})
        val _newNormalTerms = newNormalTerms.map({case Done(t) => t})
        loop(depth - 1, _newCurrTerms, normalTerms ++ _newNormalTerms)
      }
    }
    val (currTerms, normalTerms) = loop(depth, Seq(term), Seq())
    currTerms ++ normalTerms
  }

  def search(rules: Seq[SimpleRewrite], term: SimplePattern): Seq[SimplePattern] = {
    searchDepth(-1)(rules, term)
  }

  // TODO: not thread safe
  private val eqs: mutable.Map[Sort, Symbol] = mutable.Map()
  def eq(sort: Sort): Symbol = {
    if (eqs.contains(sort)) eqs(sort)
    else {
      val symbol = BOOL.eq(sort)
      eqs.put(sort, symbol)
      symbol
    }
  }

  // [ (t1,t2), (u1,u2), ... ] => t1 = t2 /\ u1 = u2 /\ ...
  def and(tts: Seq[(Term,Term)]): Term = {
    tts.map(tt => eq(tt._1.sort)(tt._1,tt._2))
      .foldLeft(BOOL(true).asInstanceOf[Term])((b,t) => BOOL.and(b,t))
  }

}
