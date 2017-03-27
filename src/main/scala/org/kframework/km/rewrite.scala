package org.kframework.km

object rewrite {

  import term._
  import builtin._
  import unification._

  def applyRule(cnt: Int)(rule: SimpleRewrite, term: SimplePattern): Seq[SimplePattern] = {
    // rule:  l => r if c
    lazy val l = rule.l.rename(cnt)
    lazy val r = rule.r.rename(cnt)
    lazy val c = rule.c.rename(cnt)
    // term:  t /\ p
    val t = term.term
    val p = term.constraint

    unify(l,t) match {
      case None => Seq()
      case Some(u) =>
        val _c = c.subst(u.subst)
        val _p = p.subst(u.subst)
        val _c_p_u = BOOL.and(Seq(BOOL.and(Seq(_c, _p)), u.constraint))

        if (sat(_c_p_u)) {
          val _r = r.subst(u.subst)
          Seq(SimplePattern(_r, _c_p_u))
        } else {
          Seq()
        }
    }
  }

  def applyRules(cnt: Int)(rules: Seq[SimpleRewrite], term: SimplePattern): Seq[SimplePattern] = {
    rules.flatMap(rule => applyRule(cnt)(rule, term))
  }

  // TODO:
  def sat(term: Term): Boolean = true

}
